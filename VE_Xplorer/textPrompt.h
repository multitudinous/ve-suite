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
 * File:          $RCSfile: textPrompt.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXT_PROMPT_H
#define TEXT_PROMPT_H

#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfText.h>
#include <Performer/pr/pfString.h>
#include <Performer/pfdu.h>
#include <Performer/pf.h>
#include <Performer/pf/pfGeode.h>    //for test

//#include <Input/InputManager/vjPosInterface.h>
//#include <Input/InputManager/vjDigitalInterface.h>

#define TEXT_DIST 1.0

class textPrompt
{
 public:
    textPrompt();   //constructor
    ~textPrompt();  //destructor
    pfDCS *add_text(char *text_input);
    
    void DeleteText();
   void flush_text(char * t);
    
        
 private:
    pfDCS *text_DCS;
    pfText *text;
    pfString *str;
    pfGeode *test;
    //pfFont *fnt;

    //vjPosInterface head;
    //vjMatrix* head_Mat;
    //pfMatrix *text_nav,*text_rot;
    //vjVec3 head_Vec;
    //vjVec3 head_pos;
};

#endif
