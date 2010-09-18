/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

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
// vim:ts=4:sw=4:et:tw=0
