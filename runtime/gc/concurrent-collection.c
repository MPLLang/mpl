void forwardChunk (GC_state s, objptr *opp, void* rawArgs) {
	objptr op = *opp;
	// pointer p = objptrToPointer (op, NULL);
	ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

	HM_chunk cand_chunk = HM_getChunkOf(objptrToPointer(op, NULL));
	if(isCandidateChunk(cand_chunk, args->origList)) {
		forwardScanRec(cand_chunk, args->repList);
	}
}

void forwardScanRec(HM_chunk chunk, void* rawArgs) {
	
	ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

	HM_markChunk(chunk) = true;
	HM_unlinkChunk(args->origList, chunk);
	HM_appendChunk(args->repList, chunk);

	pointer q = HM_getChunkStart(chunk);

	while (q != HM_getChunkFrontier(chunk)) {
	    q = foreachObjptrInObject(s, q, FALSE, trueObjptrPredicate, NULL, 
								forwardChunk, &args);
	}
}

bool isCandidateChunk(HM_chunk chunk, HM_chunkList list) {
	// Check that this chunk belongs to this list. 
	// I think we should path compress here because we will access the 
	// levelHead fairly regularly during collection.
	if(HM_getLevelHeadPathCompress(chunk)->chunkList == list) {
		return !(HM_isChunkMarked(chunk));
	}
	return false;
}

void resetChunkList(repList) {
	HM_chunk chunk = repList->firstChunk;
	while (NULL != chunk) {
		assert(HM_isChunkMarked(chunk));
		HM_unmarkChunk(chunk);
		chunk = chunk->nextChunk;
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
		forwardChunk(s, q, &args);
	}

	resetChunkList(repList);
	
	// Free the chunks in the original list
	HM_appendChunkList(getFreeListSmall(s), origList);

	// Update the original list
	origList->firstChunk = repList->firstChunk; 
	origList->lastChunk = repList->lastChunk; 
	origList->size = repList->size; 
}













