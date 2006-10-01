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
 * Date modified: $Date: 2006-07-08 22:57:46 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4907 $
 * Author:        $Author: mccdo $
 * Id:            $Id: cfdScalarBarActor.cxx 4907 2006-07-09 03:57:46Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/DataSetScalarBar.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"

#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"

#include <osg/Node>
#include <osg/Geode>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/Array>
#include <osg/LineWidth>
#include <osg/MatrixTransform>
#include <osgText/Font>
#include <osgText/Text>

#include <fstream>
#include <sstream>
#include <iomanip>
#include <string>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
////////////////////////////////////////////////////////////////////////////////
DataSetScalarBar::DataSetScalarBar( void )
{
   vprDEBUG(vesDBG,2) << "constructing cfdScalarBarActor" 
                          << std::endl << vprDEBUG_FLUSH;
   //bbox[ 0 ] = bbox[ 2 ] = bbox[ 4 ] = 0.0f;
   //bbox[ 1 ] = bbox[ 3 ] = bbox[ 5 ] = 1.0f;
   
   //xAxisLabel = "X Axis";
   //yAxisLabel = "Y Axis";
   //zAxisLabel = "Z Axis";
   scalarBarDCS = new cfdDCS();
}
////////////////////////////////////////////////////////////////////////////////
DataSetScalarBar::~DataSetScalarBar()
{
   vprDEBUG(vesDBG,2) << "deconstructing cfdScalarBarActor"
                          << std::endl << vprDEBUG_FLUSH;

   // may note need to delete anything
   vprDEBUG(vesDBG,2) << "   finished deconstructing cfdScalarBarActor"
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void DataSetScalarBar::CreateAxis( void )
{
   //Now add the labels
   osg::MatrixTransform* tempDCS = dynamic_cast< osg::MatrixTransform* >( scalarBarDCS->GetRawNode() );
   size_t numChildren = tempDCS->getNumChildren();
   for ( size_t i = 0; i < numChildren; ++i )
   {
      //we always remove 0 because once we start removing there are no longer
      // n number of children but 1 less so always remove 0
      //osg::Node* tempNode = tempDCS->getChild( 0 );
      tempDCS->removeChild( 0, 1 );
      //delete tempNode;
   }
   
   //tempGroup->addChild( CreateAxisLabels( xAxisLabel, bbox[ 1 ], bbox[ 2 ], bbox[ 4 ] ).get() );
   //tempGroup->addChild( CreateAxisLabels( yAxisLabel, bbox[ 0 ], bbox[ 3 ], bbox[ 4 ] ).get() );
   //tempGroup->addChild( CreateAxisLabels( zAxisLabel, bbox[ 0 ], bbox[ 2 ], bbox[ 5 ] ).get() );
   
   //Now add the lines
   tempDCS->addChild( CreateScalarBar().get() );
}
////////////////////////////////////////////////////////////////////////////////
cfdDCS* DataSetScalarBar::GetScalarBar( void )
{
   return scalarBarDCS;
}
//////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< ScalarBar > DataSetScalarBar::CreateScalarBar( void )
{
   cfdDataSet* activeDataSet = 0;
   if ( cfdModelHandler::instance()->GetActiveModel() )
   {
      if ( cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet() )
      {
         activeDataSet = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
      }
   }
   
   if ( activeDataSet == 0 )
   {
      return 0;
   }
   
   int activeScalarNum = activeDataSet->GetActiveScalar();
   std::string activeScalarName = activeDataSet->GetScalarName( activeScalarNum );
   
   // Create a custom color set
   std::vector<osg::Vec4> cs;
   cs.push_back(osg::Vec4(1.0f,0.0f,0.0f,1.0f));   // R
   //cs.push_back(osg::Vec4(1.0f,1.0f,0.0f,1.0f));   // G
   cs.push_back(osg::Vec4(0.0f,1.0f,0.0f,1.0f));   // G
   //cs.push_back(osg::Vec4(0.0f,1.0f,1.0f,1.0f));   // B
   cs.push_back(osg::Vec4(0.0f,0.0f,1.0f,1.0f));   // Cyan

   //Set the font
   osgSim::ScalarBar::TextProperties textProps;
   textProps._characterSize = 0.025f;
   osg::Vec4 color(1, 1, 0, 1);
   textProps._color = ( color );
   textProps._fontFile = "fonts/times.ttf";

   //setup the scalar printer
   double* scalarRange = activeDataSet->GetUserRange();
   MyScalarPrinter* myPrinter = new MyScalarPrinter();
   myPrinter->SetMinMax( scalarRange[ 0 ], scalarRange[ 1 ] );
   
   ColorRange* cr = new ColorRange( scalarRange[ 0 ], scalarRange[ 1 ], cs);
   osg::ref_ptr< ScalarBar > sb = new ScalarBar( 100, 3, cr, activeScalarName, 
                                  ScalarBar::VERTICAL, 0.1f, myPrinter );
   //sb->setScalarPrinter(new MyScalarPrinter);
   sb->setTextProperties( textProps );

   return sb;
}
