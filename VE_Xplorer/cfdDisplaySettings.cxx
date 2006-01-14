/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdDisplaySettings.h"

#include "VE_Open/VE_XML/VEDataValuePair.h"
#include "VE_Open/VE_XML/VECommand.h"

#include <jccl/RTRC/ConfigManager.h>
#include <string>

using namespace VE_Xplorer;
//////////////////////////////////////////////////////////////////////////
cfdDisplaySettings::cfdDisplaySettings( void ) 
{ 
   configuration = 0;
}
//////////////////////////////////////////////////////////////////////////
bool cfdDisplaySettings::CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray )
{
   std::string commandType;
   if ( veCommand )
   {
      commandType = veCommand->GetCommandName();
   }
   else
   {
      commandType = "wait";
   }

   if ( !commandType.compare( "Juggler_Display_Data" ) )
   {
      // Get datavalue pair from current command
      VE_XML::VEDataValuePair* commandData = veCommand->GetDataValuePair( 0 );
      double size = commandData->GetDataValue();
      std::string newCommand = commandData->GetDataName();

      // Get current list of display elements
      jccl::Configuration* oldCfg = jccl::ConfigManager::instance()->getActiveConfig();
      std::vector< jccl::ConfigElementPtr > elements;
      oldCfg->getByType( "display_window", elements );

      for ( size_t i = 0; i < elements.size(); ++i )
      {
         //std::cout << " node 1" << std::endl;
         //elements.at(0)->getNode()->save( std::cout );
         ChangeDisplayElements( true, elements.at(i) );

         if ( !newCommand.compare( "Stereo" ) )         
         {  
            // just for testing purposes can be changed to stereo later
            size+=1;
            elements.at(i)->setProperty(  "size", 1, 512 );
         }

         ChangeDisplayElements( false, elements.at(i) );
      }
   }
   return true;
}
//////////////////////////////////////////////////////////////////////////
void cfdDisplaySettings::ChangeDisplayElements( bool remove, 
                               jccl::ConfigElementPtr element )
{
   if ( configuration )
   {
      delete configuration;
   }
   configuration = new jccl::Configuration();
   
   cppdom::NodePtr nodePtr;
   configuration->createConfigurationNode( nodePtr );
   cppdom::NodePtr displayNode = element->getNode();
   nodePtr->getChild( "elements" )->addChild( displayNode );
   cppdom::NodePtr elementsNode = nodePtr->getChild( "elements" );
   std::cout << "|\t Load reconfig elements, result = " 
               << configuration->loadFromElementNode( elementsNode ) 
               << std::endl;

   if ( remove )
   {
      jccl::ConfigManager::instance()->addConfigurationRemovals( configuration );
   }
   else
   {
      jccl::ConfigManager::instance()->addConfigurationAdditions( configuration );
   }
}
