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
#include <ves/xplorer/event/data/DataSetScalarBar.h>

#include <ves/xplorer/cfdDebug.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Model.h>

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

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
////////////////////////////////////////////////////////////////////////////////
DataSetScalarBar::DataSetScalarBar( void )
{
   vprDEBUG(vesDBG,2) << "constructing cfdScalarBarActor" 
                          << std::endl << vprDEBUG_FLUSH;
   bbox[ 0 ] = bbox[ 2 ] = bbox[ 4 ] = 0.0f;
   bbox[ 1 ] = bbox[ 3 ] = bbox[ 5 ] = 1.0f;
   
   //xAxisLabel = "X Axis";
   //yAxisLabel = "Y Axis";
   //zAxisLabel = "Z Axis";
	scalarBarDCS = new ves::xplorer::scenegraph::DCS();
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
void DataSetScalarBar::SetBoundingBox( double* inBBox )
{
   for ( size_t i = 0; i < 6; ++i )
   {
      bbox[ i ] = inBBox[ i ];
   }
}
////////////////////////////////////////////////////////////////////////////////
void DataSetScalarBar::AddScalarBarToGroup( void )
{
   //Now add the labels
	osg::ref_ptr< ves::xplorer::scenegraph::DCS > tempDCS = scalarBarDCS;
   size_t numChildren = tempDCS->getNumChildren();
   for ( size_t i = 0; i < numChildren; ++i )
   {
      //we always remove 0 because once we start removing there are no longer
      // n number of children but 1 less so always remove 0
      //osg::Node* tempNode = tempDCS->getChild( 0 );
      tempDCS->removeChild( 0, 1 );
      //delete tempNode;
   }
   
   //Now add the lines
   tempDCS->addChild( CreateScalarBar().get() );
   // set position of scalar bar
   std::vector< double > trans;
   trans.push_back( bbox[ 0 ] );
   trans.push_back( bbox[ 2 ] );
   trans.push_back( bbox[ 4 ] - 0.3f );
   scalarBarDCS->SetTranslationArray( trans );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* DataSetScalarBar::GetScalarBar( void )
{
   return scalarBarDCS.get();
}
//////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< ScalarBar > DataSetScalarBar::CreateScalarBar( void )
{
   DataSet activeDataSet = 0;
   if ( ModelHandler::instance()->GetActiveModel() )
   {
      if ( ModelHandler::instance()->GetActiveModel()->GetActiveDataSet() )
      {
         activeDataSet = ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
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
   cs.push_back(osg::Vec4(0.0f,0.0f,1.0f,1.0f));   // Cyan
   cs.push_back(osg::Vec4(0.0f,1.0f,1.0f,1.0f));   // B
   cs.push_back(osg::Vec4(0.0f,1.0f,0.0f,1.0f));   // G
   cs.push_back(osg::Vec4(1.0f,1.0f,0.0f,1.0f));   // G
   cs.push_back(osg::Vec4(1.0f,0.0f,0.0f,1.0f));   // R

   //Set the font
   osgSim::ScalarBar::TextProperties textProps;
   textProps._characterSize = 0.07f;
   osg::Vec4 color(1, 1, 0, 1);
   textProps._color = ( color );
   textProps._fontFile = "fonts/times.ttf";

   //setup the scalar printer
   double* scalarRange = activeDataSet->GetDisplayedScalarRange();
   MyScalarPrinter* myPrinter = new MyScalarPrinter();
   myPrinter->SetMinMax( scalarRange[ 0 ], scalarRange[ 1 ] );
   //std::cout << scalarRange[ 0 ] << " " <<  scalarRange[ 1 ] <<std::endl;
   ColorRange* cr = new ColorRange( scalarRange[ 0 ], scalarRange[ 1 ], cs);
   osg::ref_ptr< ScalarBar > sb = new ScalarBar( 100, 3, cr, activeScalarName, 
                                  ScalarBar::VERTICAL, 0.1f, myPrinter );
   //sb->setScalarPrinter(new MyScalarPrinter);
   sb->setTextProperties( textProps );

   return sb;
}
