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
 * File:          $RCSfile: cfdTextOutput.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdTextOutput.h"

#include "cfdGeode.h"
#include "cfdDCS.h"

#include <vpr/Util/Debug.h>

#include <string>
#include <iostream>

cfdTextOutput::cfdTextOutput()
:text_DCS(0)
{
#ifdef _PERFORMER
   text = 0;
   str = 0;
#elif _OSG
#elif _OPENSG
#endif
}

cfdTextOutput::~cfdTextOutput()
{
   delete dcs;
#ifdef _PERFORMER
   pfDelete(str);
   pfDelete(text);
#elif _OSG
#elif _OPENSG
#endif
} 

cfdDCS *cfdTextOutput::add_text(char * text_input)
{
   if ( dcs )
   {
      delete dcs;
      dcs = 0;
   }
   dcs = new cfdDCS();
   dcs->SetName("Text_DCS");
#ifdef _PERFORMER
   if ( str )
   {
      pfDelete( str );
      str = 0;
   }
   str = new pfString();
   
   if ( text )
   {
      pfDelete( text );
      text = 0;
   }
   text = new pfText();
   
   pfFilePath(".:/usr/share/Performer/data");
   pfFont *fnt;
   fnt = pfdLoadFont_type1("Times-Elfin",PFDFONT_EXTRUDED);

   str->setFont(fnt);
   str->setMode(PFSTR_JUSTIFY, PFSTR_MIDDLE);
   str->setColor(0.8f,0.8f,0.0f,1.0f);
   str->setString(text_input);
   str->flatten();

   text->addString(str);
   dcs->addChild(text);
   dcs->setTrans(1.0f,0.2f,3.0f);
   dcs->setScale(0.5,0.5,0.5);

#elif _OSG
#elif _OPENSG
#endif
   return text_DCS;
}

