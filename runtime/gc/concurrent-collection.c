
bool isPointerMarked (pointer p) {
  return MARK_MASK & getHeader (p);
}

bool isInScope(HM_chunk chunk, HM_chunkList list) {
	// Check that this chunk belongs to this list.
	// I think we should path compress here because we will access the
	// levelHead fairly regularly during collection.
	if(HM_getLevelHeadPathCompress(chunk)->chunkList == list) {
		return true;
	}
	return false;
}

bool isChunkSaved(HM_chunk chunk, ConcurrentCollectArgs* args) {
	// Alternative implementation that would require a bool in the chunks
	// check if it's already added in the toSpace/saved
	// return (HM_isChunkMarked(chunk));
	return isInScope (chunk, args->repList);
}

// Mark the object uniquely identified by p
void markObj(pointer p) {
    GC_header* headerp = getHeaderp(p);
    GC_header header = *headerp;
    header ^= MARK_MASK;
    *headerp = header;
}


// void unmarkObjects (HM_chunkList repList) {
// 	HM_chunk chunk = repList->firstChunk;

//  	while (NULL != chunk) {

//  		HM_unmarkChunk(chunk);
//  		chunk = chunk->nextChunk;

// }


void saveChunk(HM_chunk chunk, void* rawArgs) {

	ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;
	HM_unlinkChunk(args->origList, chunk);
	HM_appendChunk(args->repList, chunk);
	// HM_markChunk(chunk) = true;
}

// Takes in a pointer and adds the chunk that it points to
// in the toSpace, if not added already.
void forwardPtrChunk (GC_state s, objptr *opp, void* rawArgs) {
	objptr op = *opp;
	pointer p = objptrToPointer (op, NULL);
	ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

	HM_chunk cand_chunk = HM_getChunkOf(p);
	if(isInScope(cand_chunk, args->origList)) {
		if(!isChunkSaved(cand_chunk, args)) {
			saveChunk(cand_chunk, args->repList);
		}

		// !isChunkSaved => !isPointerMarked can merge into one
		if(!isPointerMarked(p)) {
			markObj(p);
			foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
							forwardPtrChunk, &args);
		}
	}
}

// Dual unmarking function -- can merge codes by adding another param to args
void unmarkObjects(GC_state s, objptr *opp, void * rawArgs) {
	objptr op = *opp;
	pointer p = objptrToPointer (op, NULL);
	ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

	HM_chunk cand_chunk = HM_getChunkOf(p);

	if(isInScope(cand_chunk, args->repList)) {
		if(isPointerMarked(p)) {
			unmarkObj(p);
			foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
							unmarkObjects, &args);
		}
	}
}

void collectWithRoots(INIT_ROOT_SET, HM_HierarchicalHeap targetHH) {
	struct HM_chunkList _repList;
	HM_chunkList repList = &(_repList);

	HM_chunkList origList = HM_HH_getChunkList(targetHH);

	ConcurrentCollectArgs args = {
		.origList = origList,
		.repList  = repList
	}

	for(auto q: INIT_ROOT_SET) {
		callIfIsObjptr(s, forwardPtrChunk, ((objptr*)(q)), &args);
	}

	// Free the chunks in the original list
	HM_appendChunkList(getFreeListSmall(s), origList);

	for(auto q: INIT_ROOT_SET) {
		callIfIsObjptr(s, unmarkObjects, ((objptr*)(q)), &args);
	}


	// Update the original list
	origList->firstChunk = repList->firstChunk;
	origList->lastChunk = repList->lastChunk;
	origList->size = repList->size;
}
