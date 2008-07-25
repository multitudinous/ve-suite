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
