/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef WIN32

#include "SAPIVoiceInterpreter.h"
#include "VEMsgInterpreter.h"
#include "VE_XPlorer/cfdDebug.h"
#include <atlbase.h> // Just for unicode conversion macros...
#include <atlconv.h> // ... W2A, A2W
#include <shellapi.h>
#include <iostream>

SAPIVoiceInterpreter::SAPIVoiceInterpreter(int argc, char** argv) : VEVoiceInterpreter(argc, argv)
{
   bool foundGrammar = false;
   const std::string defaultGrammarFile = "voice_commands.xml";

   // For now, require the grammar file to reside in $VE_SUITE_HOME/VE_Xplorer/VoiceCommand/
   this->cmdsFile = getenv("VE_SUITE_HOME");
   cmdsFile += "\\VE_Xplorer\\VoiceCommand\\";

   // Look for relevant arguments, hope for no pathological naming
   for(int i=0;i<argc-1;i++)
   {
	  if(!strcmp(argv[i],"-SAPIGrammar"))
	  {
		 if(!foundGrammar)
		 {
			cmdsFile += argv[i+1];
			foundGrammar = true;
		 }
		 else
		 {
			vprDEBUG(vesDBG,0) << " Warning: found multiple SAPI grammar file declarations, "
			   << "using just the first one.\n" << vprDEBUG_FLUSH;
		 }
 	  }
   }
   if(!foundGrammar)
   {
	  vprDEBUG(vesDBG,2) << " Using default SAPI grammar, " << defaultGrammarFile
		 << "\n" << vprDEBUG_FLUSH;
	  cmdsFile += defaultGrammarFile;
   }
};

SAPIVoiceInterpreter::~SAPIVoiceInterpreter()
{
   if(this->grammar)
   {
	  this->grammar.Release();
   }
   CoUninitialize();
}

bool SAPIVoiceInterpreter::SAPIInit()
{
   USES_CONVERSION;
   if(!SUCCEEDED(CoInitialize(NULL))){
	  vprDEBUG(vesDBG,0) << " Failed to initialize COM, cannot use SAPI.\n" << vprDEBUG_FLUSH;
	  return false;
   }
   else if(ConfigureSAPI(this->cmdsFile) == 0)
   {
	  return true;
   }
   else
   {
	  vprDEBUG(vesDBG,0) << " Failed to initialize SAPI, stopping.\n" << vprDEBUG_FLUSH;
   }
   return false;
}

void SAPIVoiceInterpreter::Listen()
{
   // Message loop
   MSG msg;
   while( IsRunning() ) {
	  while(!PeekMessage(&msg,NULL,0,0,PM_REMOVE))
	  {
		 Sleep(10);
	  }
	  TranslateMessage(&msg);
	  DispatchMessage(&msg);
   }
}

// Static member callback
void __stdcall SAPIVoiceInterpreter::SAPICallback(WPARAM wp, LPARAM lp)
{
   USES_CONVERSION;
   CSpEvent event;
   WCHAR* res;
   std::string resAsStr;
   res = NULL;
   
   vprDEBUG(vesDBG,1) << " Caught a SAPI recognition event:\n" << vprDEBUG_FLUSH;
   while(event.GetFrom(reinterpret_cast<SAPIVoiceInterpreter*>(lp)->context) == S_OK)
   {
	  event.RecoResult()->GetText(SP_GETWHOLEPHRASE, SP_GETWHOLEPHRASE, FALSE, &res, NULL);
	  if(res)
	  {
		 resAsStr = W2A(res);
		 vprDEBUG(vesDBG,1) << "  " << resAsStr << '\n' << vprDEBUG_FLUSH;
		 reinterpret_cast<SAPIVoiceInterpreter*>(lp)->ProcessPhrase(resAsStr);
	  }
   }
}

// Main SAPI initialization
// Trying to keep all the HRESULT stuff down here
int SAPIVoiceInterpreter::ConfigureSAPI(std::string grammar_file)
{
   USES_CONVERSION;
   WCHAR* tmp_grammar_file=A2W(grammar_file.c_str());
   HRESULT ok;
   ok = this->rec.CoCreateInstance(CLSID_SpInprocRecognizer);
   if(SUCCEEDED(ok)){
      ok = this->rec->CreateRecoContext(&(this->context));
   }
   if(SUCCEEDED(ok)){
      ok = this->context->SetNotifyCallbackFunction(SAPICallback,0,reinterpret_cast<LPARAM>(this));
   }
   if(SUCCEEDED(ok)){
      ok = this->context->SetInterest(SPFEI(SPEI_RECOGNITION), SPFEI(SPEI_RECOGNITION));
   }
   if(SUCCEEDED(ok)){
      ok = this->context->CreateGrammar(0, &(this->grammar));
   }
   if(SUCCEEDED(ok)){
      ok = this->grammar->LoadCmdFromFile(tmp_grammar_file, SPLO_STATIC);
   }else{
      vprDEBUG(vesDBG,0) << " SAPI setup failed.\n" << vprDEBUG_FLUSH;
      return SUCCEEDED(ok)? 0 : 1;
   }
   if(SUCCEEDED(ok)){
      ok = this->grammar->SetRuleState(NULL,NULL,SPRS_ACTIVE);
   }else{
      vprDEBUG(vesDBG,0) << " SAPI failed to load grammar file: " << grammar_file << "\n"
		 << vprDEBUG_FLUSH;
      return SUCCEEDED(ok)? 0 : 1;
   }
   if(SUCCEEDED(ok)){
      ok = SpCreateDefaultObjectFromCategoryId(SPCAT_AUDIOIN, &(this->audio));
   }
   if(SUCCEEDED(ok)){
      ok = this->rec->SetInput(this->audio, TRUE);
   }
   if(SUCCEEDED(ok)){
      ok = this->rec->SetRecoState(SPRST_ACTIVE);
   }
   if(!SUCCEEDED(ok)){
      vprDEBUG(vesDBG,0) << " SAPI failed to find microphone.\n" << vprDEBUG_FLUSH;
   }
   return SUCCEEDED(ok)? 0 : 1;
}

#endif // WIN32
