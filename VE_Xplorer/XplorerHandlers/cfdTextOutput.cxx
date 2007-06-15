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
#include "VE_Xplorer/XplorerHandlers/cfdTextOutput.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include "VE_Xplorer/SceneGraph/Group.h"

#include <string>
#include <iostream>

#ifdef _PERFORMER
#include <Performer/pfdu.h>
#include <Performer/pf/pfDCS.h>
#elif _OSG
#elif _OPENSG
#endif

using namespace VE_SceneGraph;
using namespace VE_Xplorer;

cfdTextOutput::cfdTextOutput()
:dcs(0)
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
   //delete dcs;
#ifdef _PERFORMER
   pfDelete(str);
   pfDelete(text);
#elif _OSG
#elif _OPENSG
#endif
} 

VE_SceneGraph::DCS* cfdTextOutput::add_text(std::string text_input)
{
   if ( dcs.valid() )
   {
		VE_SceneGraph::Group* temp = dynamic_cast< VE_SceneGraph::Group* >(dcs->GetParent( 0 ));
      if ( temp )
      {
         temp->RemoveChild( dcs.get() );
      }
   }
   dcs = new VE_SceneGraph::DCS();
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
   str->setString(text_input.c_str());
   str->flatten();
   //str->getGState();
   text->addString(str);
   ((pfDCS*)dcs->GetRawNode())->addChild(text);

   float trans[ 3 ];
   trans[ 0 ] = 1.0f;
   trans[ 1 ] = 0.2f;
   trans[ 2 ] = 3.0f;
   dcs->SetTranslationArray( trans );

   float scale[ 3 ];
   scale[ 0 ] = 0.5f;
   scale[ 1 ] = 0.5f;
   scale[ 2 ] = 0.5f;
   dcs->SetScaleArray( scale );
#elif _OSG
#elif _OPENSG
#endif
   return dcs.get();
}

