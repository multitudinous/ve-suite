/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: textPrompt.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifdef _PERFORMER
#include <vpr/Util/Debug.h>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include "textPrompt.h"

textPrompt::textPrompt()
	: text_DCS(0), text(0), str(0), test(0) 
{
    //str = new pfString();
    //text = new pfText();
    //text_DCS = new pfDCS();
}

textPrompt::~textPrompt()
{
 pfDelete(str);
 pfDelete(text);
 pfDelete(text_DCS);
  } 

pfDCS *textPrompt::add_text(char * text_input)
{
   pfString *str = new pfString();
   pfText *text = new pfText();
   pfDCS *text_DCS = new pfDCS();
   
   pfFilePath(".:/usr/share/Performer/data");
   pfFont *fnt;
   fnt = pfdLoadFont_type1("Times-Elfin",PFDFONT_EXTRUDED);

   str->setFont(fnt);
   str->setMode(PFSTR_JUSTIFY, PFSTR_MIDDLE);
   str->setColor(0.8f,0.8f,0.0f,1.0f);
   str->setString(text_input);
   str->flatten();

   text->addString(str);
   text_DCS->addChild(text);
   text_DCS->setTrans(1.0f,0.2f,3.0f);
   text_DCS->setScale(0.5,0.5,0.5);

   return text_DCS;

}

/*pfDCS *textPrompt::getTxDCS()
{
  vprDEBUG(vprDBG_ALL, 0) <<"TTT\n"<< endl << vprDEBUG_FLUSH;
  return text_DCS;
}*/

/*void textPrompt::GetXform( )
{
  head_Vec.set( 0.0f, 0.0f, 1.0f );
  head_Mat = head->getData( );
  head_Mat->getTrans(head_pos[0],head_pos[1],head_pos[2]);
  head_Vec.xformVec( *(head_Mat), head_Vec );
  head_Vec.normalize();
   vprDEBUG(vprDBG_ALL, 0) 
      <<head_pos[0]<<","<<head_pos[1]<<","<<head_pos[2]<<"\n"<< std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 0)
      <<head_Vec[0]<<","<<head_Vec[1]<<","<<head_Vec[2]<<"\n"<< std::endl << vprDEBUG_FLUSH;  //for debugging
}

float textPrompt::GetTextRot( )
{
  float deg = atan2f(head_Vec[0],head_Vec[2])*180/PI;
  vprDEBUG(vprDBG_ALL, 0) << deg << "\n" << std::endl << vprDEBUG_FLUSH;
  return(deg);
}


pfMatrix textPrompt::GetHdDir()
{
  float t;
  this->GetXform();

  //text_rot->makeIdent();
  t = this->GetTextRot();
  vprDEBUG(vprDBG_ALL, 0) << t << "\n" << std::endl << vprDEBUG_FLUSH;
  text_rot->makeRot(t,0.0,0.0,1.0);  //Rotate the text according to head's direction

  text_nav->makeIdent();
  //text_nav->makeTrans(head_pos[0]-(head_Vec[0]*TEXT_DIST),-head_pos[2]+(head_Vec[2]*TEXT_DIST),head_pos[1]-(head_Vec[1]*TEXT_DIST));

  //text_rot->postMult(*text_nav);
  
  return(*text_rot);
}     for moving text   --job deferred */


void textPrompt::DeleteText()
{
  pfDelete(this->text_DCS);
  pfDelete(this->text);
  pfDelete(this->str);
}

void textPrompt::flush_text(char * t)
{
   return;
    /*-- for 3D pop-up text message
      set add_text signal to activate text prompt:
      0 = no output(initial state)
      1 = add text to output(without timer)
      2 = add text to output(with timer)
      3 = hold the output(with timer)
      4 = delete text from output
      6 = hold the output(without timer)
      9 = not allow text output unless re-select the menu*/
/*      
   my_timer = time(&my_timer);

   if (text_sig == 1)
   {
      this->temp_text = this->tPrompt->add_text(t);
      this->rootNode->InsertChild(3,this->temp_text);
      vprDEBUG(vprDBG_ALL,1) << "TTT" << std::endl << vprDEBUG_FLUSH;
      //this->tPrompt->DeleteText();
      text_sig = 6;
   }

   if (text_sig == 2)
   {
      this->temp_text = this->tPrompt->add_text(t);
      this->rootNode->insertChild(3,this->temp_text);
      //this->tPrompt->DeleteText();
      text_sig = 3;     //  3 = hold the output(with timer)
   }

   if ( (my_timer - time_r) > 5 && text_sig == 3)
   {
      text_sig = 4;
   }

   if (text_sig == 4)    // 4 = delete text from output
   { 
      int text_num =  rootNode->searchChild(this->temp_text);
      if ( text_num != -1)
      {
         temp_text = this->rootNode->getChild(text_num);
         this->rootNode->removeChild(temp_text);
         pfDelete(temp_text);
         //this->tPrompt->DeleteText();
      }
      else
      {
         vprDEBUG(vprDBG_ALL,1) << "in cfdApp: no text to delete" 
                                << std::endl << vprDEBUG_FLUSH;
      }
      text_sig = 9;
   }
*/
}
#elif _OSG
#elif _OPENSG
#endif
