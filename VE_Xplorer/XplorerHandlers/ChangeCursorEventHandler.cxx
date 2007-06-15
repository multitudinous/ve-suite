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
#include "VE_Xplorer/XplorerHandlers/ChangeCursorEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdCursor.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler::ChangeCursorEventHandler()
:VE_EVENTS::EventHandler()
{
   _activeModel = 0;
}
////////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler::ChangeCursorEventHandler(const ChangeCursorEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler::~ChangeCursorEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler& ChangeCursorEventHandler::operator=(const ChangeCursorEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::ChangeCursorEventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ChangeCursorEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   try
   {
      if ( model )
      {
         _activeModel = dynamic_cast< VE_Xplorer::cfdModel* >( model );
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to ChangeCursorEventHandler::SetGlobalBaseObject!"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////
void ChangeCursorEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
   //Grab the subdialog settings from streamlines to adjust cursor settings
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePair* activeModelDVP = command->GetDataValuePair( "Sub-Dialog Settings" );
   VE_XML::Command* objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );
   if ( objectCommand->GetCommandName() != "UPDATE_STREAMLINE_SETTINGS" )
   {
      return;
   }

   std::string direction;
   VE_XML::DataValuePair* directionDVP = objectCommand->GetDataValuePair( "Cursor Direction" );
   if ( directionDVP )
   {
      directionDVP->GetData( direction );
   }
   
   std::string planes;
   VE_XML::DataValuePair* planesDVP = objectCommand->GetDataValuePair( "Cursor Type" );
   if ( planesDVP )
   {
      planesDVP->GetData( planes );      
   }
   
   unsigned int numPointsPerPlane = 2;
   VE_XML::DataValuePair* pointsDVP = objectCommand->GetDataValuePair( "Number Of Points Per Plane" );
   if ( pointsDVP )
   {
      pointsDVP->GetData( numPointsPerPlane );
      std::cout << " num points " << numPointsPerPlane << std::endl;
      cfdEnvironmentHandler::instance()->GetCursor()->SetPlaneReso( static_cast< int >( numPointsPerPlane ) );
   }
   
   double planeSize = 1;
   VE_XML::DataValuePair* sizeDVP = objectCommand->GetDataValuePair( "Size" );
   if ( sizeDVP )
   {
      sizeDVP->GetData( planeSize );      
      std::cout << " planesize " << planeSize << std::endl;
      cfdEnvironmentHandler::instance()->GetCursor()->SetPlaneSize( static_cast< int >( planeSize ) );
   }
   

   if ( planes == "plane" )
   {
      if ( direction == "x" )
      {
         cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( XPLANE );
      }
      else if ( direction == "y" )
      {
         cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( YPLANE );
      }
      else if ( direction == "z" )
      {
         cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( ZPLANE );
      }
   }
   else if ( planes == "none" )
   {
      cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( NONE );
   }
   else if ( planes == "point" )
   {
      cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( SPHERE );
   }
   else if ( planes == "line" )
   {
      if ( direction == "x" )
      {
         cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( XLINE );
      }
      else if ( direction == "y" )
      {
         cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( YLINE );
      }
      else if ( direction == "z" )
      {
         cfdEnvironmentHandler::instance()->GetCursor()->SetCursorType( ZLINE );
      }
   }

   /*std::string advanced;
   VE_XML::DataValuePair* advancedDVP = objectCommand->GetDataValuePair( "Advanced Scalar Settings" );
   if ( advancedDVP )
   {
      VE_XML::Command* advancedCommand = dynamic_cast< VE_XML::Command* >( advancedDVP->GetDataXMLObject() );
      unsigned int warpOption = 0;
      advancedCommand->GetDataValuePair( "Warp Option" )->GetData( warpOption );
      if ( warpOption )
         advanced = "-warp";
   }*/
}
