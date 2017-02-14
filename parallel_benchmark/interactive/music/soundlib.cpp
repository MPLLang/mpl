#include <SFML/System/InputStream.hpp>
#include <SFML/Audio/Music.hpp>
#include <SFML/Audio/SoundBuffer.hpp>
#include <SFML/Audio/Sound.hpp>
#include <SFML/System.hpp>
#include <string.h>
#include <stdio.h>
#include <cstdio>
#include <unistd.h>
#include "sound.h"

class SMLStream : public sf::InputStream {

public:
  sf::Mutex *mutex;
  char *alldata;
  sf::Int64 size;
  sf::Int64 readpos;
  volatile sf::Int64 currpos;
  SMLStream (sf::Int64 sz, char *init, sf::Int64 initsz) {
    readpos = 0;
    size = sz;
    alldata = (char *)malloc(sz);
    memcpy(alldata, init, initsz);
    currpos = initsz;
    mutex = new sf::Mutex();
  }
  ~SMLStream() {
  }
  sf::Int64 read (void *data, sf::Int64 size) {
    mutex->lock();
    sf::Int64 totlen = 0;
    int prevpos = currpos;
    printf ("Reading %d bytes\n", size);
    while (readpos + size > currpos) {
      printf("stalled\n");
    }
    int i = 0;
    //for (i = readpos; i < readpos + 100; i++)
    //  printf ("%c", alldata[i]);
    sf::Int64 bytesread = std::min(size, prevpos + totlen - readpos);
    //printf("bytesread: %d, readpos: %d, currpos: %d\n", (int)bytesread, (int)readpos, (int)currpos);
    memcpy((char *)data, alldata + readpos, bytesread);
    readpos += bytesread;
    //printf("bytesread: %d, readpos: %d, currpos: %d\n", (int)bytesread, (int)readpos, (int)currpos);
    mutex->unlock();
    return bytesread;
  }
  sf::Int64 seek (sf::Int64 position) {
    mutex->lock();
    readpos = position;
    mutex->unlock();
    return readpos;
  }
  sf::Int64 tell () {
    return readpos;
  }
  sf::Int64 getSize() {return size;}
};

class SMLMusic {
public:
  SMLStream *s;
  sf::Music *music;

  SMLMusic(sf::Int64 sz, char *init, sf::Int64 initsz) {
    printf("Starting to initialize\n");
    s = new SMLStream(sz, init, initsz);
    music = new sf::Music;

    if(!music->openFromStream(*s)) {
      //if (!music->openFromMemory(data, totsize)) {
      //if (!music->openFromFile("nice_music.ogg")) {
      delete music;
      fail ();
    }
    printf("Initialized\n");
  }

  ~SMLMusic() {}
};

// I know this isn't how OO works, but the MLton FFI isn't set up for C++

extern "C" SMLMusic *SMLMusicInit(sf::Int64 sz, char *init, sf::Int64 initsz) {
  return new SMLMusic(sz, init, initsz);
}

extern "C" void SMLMusicPlay(SMLMusic *m) {
  m->music->play();
}

extern "C" void SMLMusicPause(SMLMusic *m) {
  m->music->pause();
}

extern "C" void SMLMusicAddData(SMLMusic *m, char *data, sf::Int64 sz) {
  memcpy(m->s->alldata + m->s->currpos, data, sz);
  m->s->currpos += sz;
}
