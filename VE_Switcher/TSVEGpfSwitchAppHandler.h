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
 * File:          $RCSfile: TSVEGpfSwitchAppHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef TSVEG_PF_SWITCH_APP_HANDLER_H_
#define TSVEG_PF_SWITCH_APP_HANDLER_H_

#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfGroup.h>
#include "cfdNavigate.h"

class pfBinaryFiles
{
public:

   //Constructor sets up a character array
   pfBinaryFiles( void );

   char binFiles[100];
   float pfBinScale;
};


class pfAppHandle
{
public:

   //Contructor grabs the rootnode and declares a new pfDCS
   pfAppHandle( pfGroup *temp );
   
   //Lets the user set the icon scale of the app
   void setIconScale(float);

   //Adds the model to the pfDCS
   void addToGraph(pfNode *);

   //Lets the app add itself to the rootnode
   void addDCSToRoot( void );

   //Lets the app take itself off of the rootnode
   void removeDCSFromRoot( void );

   //Steps into the pfDCS
   void selectApp( float );

   //Steps out of the pfDCS
   void deSelectApp( float );

   //Controls z-axis translation
   void setPos(float);

   //Translates the next app into the room  
   void bringAppIn( float );

   //Translates the previous app out of the room
   void pushAppOut( float );

   //Controls the apps position 
   void setPosAllAxis(float, float, float);

private:
   pfGroup *rootNode;
   pfDCS* thisApp;
   float iconScale;

};
#endif

