
#ifndef VE_SPEECH_RECOGNITION_OBSERVER_H_
#define VE_SPEECH_RECOGNITION_OBSERVER_H_

#include <boost/shared_ptr.hpp>

#include <string>

/**
 * Interface for a Speech Recognition Observer; this observer will be 
 * notified whenever a speech recognition event occurs.
 */
class SpeechRecognitionObserver
{
public:
   SpeechRecognitionObserver()
   {}

   virtual ~SpeechRecognitionObserver()
   {}

   virtual void onPhraseRecognition(const std::string& phrase) = 0;

   virtual void onFailedRecognition()
   {
   }

};

typedef boost::shared_ptr<SpeechRecognitionObserver> SpeechRecognitionObserverPtr;

#endif
