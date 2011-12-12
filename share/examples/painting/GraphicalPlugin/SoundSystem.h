// Name: VirtualPaint Demo
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: June 21, 2011

// Configurable options
//#define SHOW_MODEL

// Platform includes
#ifdef WIN32
#include <windows.h>
typedef unsigned long VCAS_LONG;
#else
typedef unsigned int VCAS_LONG;
#endif
/*
#include <osgAudio/SoundManager.h>
#include <osgAudio/Source.h>
#include <osgAudio/Sample.h>
#include <osgAudio/AudioEnvironment.h>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;


// --------------------------------------------------------------------------
// A holder class for all audio-related things.

class SoundSystem
{
   public:
      SoundSystem() : sprayLoaded(false) {Init();};

      void StartSpray() {if (sprayLoaded) spraySnd->play();}
      // stop the sound playback, and make sure it starts at the beginning next time.
      void StopSpray() {if (sprayLoaded) spraySnd->stop(); spraySnd->rewind();}

   protected:
      osg::ref_ptr<osgAudio::Source> spraySnd;

   private:
      void Init()
         {
         osgAudio::AudioEnvironment::instance()->init(10);
         spraySnd = new osgAudio::Source;
         if (spraySnd.valid() == false)
            return;
         spraySnd->stop();    // WHY IS THIS NEEDED?!?
         std::string wavFileName = osgDB::findDataFile("sounds/SpraySnd.wav");
         osg::ref_ptr<osgAudio::Sample> sndSample = new osgAudio::Sample(wavFileName);
         if (sndSample.valid() == false)
            return;
         spraySnd->setSound(sndSample.get());
         spraySnd->setLooping();
         spraySnd->setAmbient();
         spraySnd->setGain(1.0f);
         sprayLoaded = true;
         }

      bool sprayLoaded;
};

*/