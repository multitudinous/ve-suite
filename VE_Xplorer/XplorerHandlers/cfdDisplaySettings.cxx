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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdDisplaySettings.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"

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

   if ( commandType.compare( "Juggler_Display_Data" ) == 0)
   {
      // Get datavalue pair from current command
      VE_XML::DataValuePair* commandData = veCommand->GetDataValuePair( 0 );
      double stereoToggle = commandData->GetDataValue();
      std::string newCommand = commandData->GetDataName();

      // Get current list of display elements
      jccl::Configuration* oldCfg = jccl::ConfigManager::instance()->getActiveConfig();
      std::vector< jccl::ConfigElementPtr > elements;
      oldCfg->getByType( "display_window", elements );

      for ( size_t i = 0; i < elements.size(); ++i )
      {
         //elements.at(0)->getNode()->save( std::cout );
         ChangeDisplayElements( true, elements.at(i) );

         if ( stereoToggle == 1.0f )
         {  
            // just for testing purposes can be changed to stereo later
            elements.at(i)->setProperty(  "stereo", 0, stereoToggle );
            size_t numSvPtrs = elements.at(i)->getNum( "surface_viewports" );
            for ( size_t j = 0; j < numSvPtrs; ++j )
            {
               jccl::ConfigElementPtr svPtr = elements.at( i )->getProperty< jccl::ConfigElementPtr >( "surface_viewports", j );
               svPtr->setProperty(  "view", 0, std::string( "Both Eyes" ) );
            }
         }
         else
         {
            elements.at(i)->setProperty(  "stereo", 0, stereoToggle );
            size_t numSvPtrs = elements.at(i)->getNum( "surface_viewports" );
            for ( size_t j = 0; j < numSvPtrs; ++j )
            {
               jccl::ConfigElementPtr svPtr = elements.at( i )->getProperty< jccl::ConfigElementPtr >( "surface_viewports", j );
               svPtr->setProperty(  "view", 0, std::string( "Left Eye" ) );
            }
         }

         ChangeDisplayElements( false, elements.at(i) );
      }
   }
   else if ( commandType.compare( "Juggler_Desktop_Data" ) == 0 )
   {
      jccl::ConfigManager::instance()->lockActive();
      // Get current list of display elements
      jccl::Configuration* oldCfg = jccl::ConfigManager::instance()->getActiveConfig();
      std::vector< jccl::ConfigElementPtr > elements;
      oldCfg->getByType( "display_window", elements );

      for ( size_t i = 0; i < elements.size(); ++i )
      {
         ChangeDisplayElements( true, elements.at(i) );

         //Process the resolution
         VE_XML::DataValuePair* desktopData = veCommand->GetDataValuePair( "desktop_width" );
         // 2/3 the width
         int xSize = static_cast< int >( desktopData->GetDataValue() * 0.667f ); 
         elements.at(i)->setProperty(  "size", 0, xSize );
         desktopData = veCommand->GetDataValuePair( "desktop_height" );
         // 50 for the menu bar height
#ifdef WIN32
         int ySize = static_cast< int >( desktopData->GetDataValue() - 160 ); 
#else
         int ySize = static_cast< int >( desktopData->GetDataValue() - 195 );
#endif
         elements.at(i)->setProperty(  "size", 1, ySize );
         elements.at(i)->setProperty(  "origin", 0, 0 );
         elements.at(i)->setProperty(  "origin", 1, 0 );
         //Process the physical corners of the window
         size_t numSvPtrs = elements.at(i)->getNum( "surface_viewports" );
         for ( size_t j = 0; j < numSvPtrs; ++j )
         {
            jccl::ConfigElementPtr svPtr = elements.at( i )->getProperty< jccl::ConfigElementPtr >( "surface_viewports", j );
            // process x first
            double xmin = svPtr->getProperty< double >(  "lower_left_corner", 0 );
            double xmax = svPtr->getProperty< double >(  "lower_right_corner", 0 );
            double xScreenDim = xmax - xmin;
            //this constant number is meters / pixel
            double newXScreenDim = 0.0019050f * xSize;
            double xScreenDif = newXScreenDim - xScreenDim;
            double xScreenDifHalf = xScreenDif * 0.5f;
            double newXmin = xmin - xScreenDifHalf;
            double newXmax = xmax + xScreenDifHalf;
         
            svPtr->setProperty(  "lower_left_corner", 0, newXmin );
            svPtr->setProperty(  "lower_right_corner", 0, newXmax );
            svPtr->setProperty(  "upper_left_corner", 0, newXmin );
            svPtr->setProperty(  "upper_right_corner", 0, newXmax );
            // now process y
            double ymin = svPtr->getProperty< double >(  "lower_left_corner", 1 );
            double ymax = svPtr->getProperty< double >(  "upper_left_corner", 1 );
            double yScreenDim = ymax - ymin;
            //this constant number is meters / pixel
            double newYScreenDim = 0.001786f * ySize;
            double yScreenDif = newYScreenDim - yScreenDim;
            double yScreenDifHalf = yScreenDif * 0.5f;
            double newYmin = ymin - yScreenDifHalf;
            double newYmax = ymax + yScreenDifHalf;
         
            svPtr->setProperty(  "lower_left_corner", 1, newYmin );
            svPtr->setProperty(  "lower_right_corner", 1, newYmin );
            svPtr->setProperty(  "upper_left_corner", 1, newYmax );
            svPtr->setProperty(  "upper_right_corner", 1, newYmax );
         }

         ChangeDisplayElements( false, elements.at(i) );
      }
      jccl::ConfigManager::instance()->unlockActive();
   }
   veCommand = 0;
   return true;
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
