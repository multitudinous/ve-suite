/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: textPrompt.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXT_PROMPT_H
#define TEXT_PROMPT_H
#ifdef _PERFORMER
#include <Performer/pf/pfText.h>
#include <Performer/pr/pfString.h>
#elif _OSG
#elif _OPENSG
#endif

class cfdGeode;
class cfdDCS;

class cfdTextOutput
{
   public:
      cfdTextOutput();   //constructor
      ~cfdTextOutput();  //destructor
      cfdDCS *add_text(char *text_input);
    
   private:
      cfdDCS*   dcs;
#ifdef _PERFORMER
      //pfFont *fnt;
      pfText *text;
      pfString *str;
#elif _OSG
#elif _OPENSG
#endif
};

#endif // TEXT_PROMPT_H
