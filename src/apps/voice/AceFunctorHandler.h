/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef VE_VOICE_ACE_FUNCTOR_HANDLER_H_
#define VE_VOICE_ACE_FUNCTOR_HANDLER_H_

#include <ace/Event_Handler.h>

#include <loki/Functor.h>
#include <loki/Typelist.h>
#include <loki/TypelistMacros.h>

/**
 * Provides a Functor adaptor to the ACE Event Handler interface.  This allows
 * one to use Functors for ACE Event Handlers instead of a lot of subclasses.
 */
class AceFunctorHandler : public ACE_Event_Handler
{
public:
   typedef Loki::Functor<int, LOKI_TYPELIST_3(int, siginfo_t*, ucontext_t*)> 
      SignalHandler;

   AceFunctorHandler()
   {
   }

   ~AceFunctorHandler()
   {
   }

   /**
    * Sets the signal handler for this event handler object.
    *
    * @param   handler     the functor to invoke on a handle_signal call.
    */
   void setSignalHandler(const SignalHandler& handler)
   {
      mSignalHandler = handler;
   }

   int handle_signal(int signum, siginfo_t* si, ucontext_t* ut)
   {
      if (!mSignalHandler.empty())
      {
         return mSignalHandler(signum, si, ut);
      }
      return 0;
   }

private:

   /// The functor that is invoked when a signal event occurs.
   SignalHandler                                mSignalHandler;
};

#endif
