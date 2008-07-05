
#ifndef VE_SPEECH_RECOGNITION_OBSERVER_H_
#define VE_SPEECH_RECOGNITION_OBSERVER_H_

#include <loki/SmartPtr.h>
#include <loki/Functor.h>
#include <loki/Typelist.h>
#include <loki/TypelistMacros.h>

#include <string>

/**
 * This observer is notified anytime a phrase recognition and failed 
 * recognition event occurs.  It works a little differently than the standard
 * observer pattern by using functors instead of polymorphism.
 */
class SpeechRecognitionObserver
{
public:

   /// The type of functor that will be invoked for phrase recognition events.
   typedef Loki::Functor<void, LOKI_TYPELIST_1(const std::string&)>
      PhraseRecognitionHandler;

   /// The type of functor that will be invoked for failed recognition events.
   typedef Loki::Functor<void>
      FailedRecognitionHandler;

   /**
    * Initializes this SpeechRecognitionObserver with the given phrase
    * recognition functor.
    *
    * @param   phraseHandler     the functor to invoke when a phrase is
    *                            recognized.
    */
   SpeechRecognitionObserver(const PhraseRecognitionHandler& phraseHandler)
      : mPhraseHandler(phraseHandler)
   {}

   /**
    * Initializes this SpeechRecognitionObserver with the given functors.
    *
    * @param   phraseHandler     the functor to invoke when a phrase is
    *                            recognized.
    * @param   failedHandler     the functor to invoke when a phrase
    *                            recognition failure occurs.
    */
   SpeechRecognitionObserver(const PhraseRecognitionHandler& phraseHandler,
                             const FailedRecognitionHandler& failedHandler)
      : mPhraseHandler(phraseHandler), mFailedHandler(failedHandler)
   {}

   /* Using Default Dtor */ 
   
   /**
    * Called by the Subject whenever a phrase recognition event occurs.
    * This will forward the event to the phrase handler functor.
    *
    * @param   phrase      the phrase that was recognized.
    */
   void onPhraseRecognition(const std::string& phrase)
   {
      if (!mPhraseHandler.empty())
      {
         mPhraseHandler(phrase);
      }
   }

   /**
    * Called by the Subject whenever a failed phrase recognition occurs.
    * This will forward the event to the failed phrase handler.
    */
   void onFailedRecognition()
   {
      if (!mFailedHandler.empty())
      {
         mFailedHandler();
      }
   }

private:

   /// The functor that is invoked for phrase recognition events.
   PhraseRecognitionHandler                              mPhraseHandler;

   /// The functor that is invoked for failed speech recognition events.
   FailedRecognitionHandler                              mFailedHandler;
};

typedef Loki::SmartPtrDef<SpeechRecognitionObserver>::type 
   SpeechRecognitionObserverPtr;

#endif
