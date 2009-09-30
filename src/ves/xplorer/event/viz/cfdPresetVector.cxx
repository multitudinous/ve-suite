/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/communication/CommandHandler.h>
#include <ves/xplorer/event/viz/cfdPresetVector.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/event/viz/cfdCuttingPlane.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/Command.h>

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkCellDataToPointData.h>
#include <vtkPassThroughFilter.h>
#include <vtkXMLPolyDataWriter.h>
#include <ves/xplorer/event/viz/OSGStage.h>


using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

// this class requires that the dataset has a vector field.
cfdPresetVector::cfdPresetVector( const int xyz, const int numSteps )
{
    this->xyz = xyz;
    this->numSteps = numSteps;
}

cfdPresetVector::~cfdPresetVector()
{
    ;
}

void cfdPresetVector::Update( void )
{

	// temporary Boolean variable to
	// test the GPU based rendering    
	bool gpustuff = false;   

 
    vprDEBUG( vesDBG, 1 ) << "|\tcfdPresetVector::ActiveDataSet = "
        << this->GetActiveDataSet()
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\t\tcfdPresetVector "<< this->cursorType
        << " : " << usePreCalcData
        << std::endl << vprDEBUG_FLUSH;

    if( this->usePreCalcData && ( xyz < 3 ) )
    {

        cfdPlanes* precomputedPlanes =
            this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz );
        if( !precomputedPlanes )
        {
            vprDEBUG( vesDBG, 0 )
            << "|\tDataset contains no precomputed vector planes."
            << std::endl << vprDEBUG_FLUSH;
            ves::xplorer::CommandHandler::instance()
            ->SendConductorMessage( "Dataset contains no precomputed vector planes.\n" );
            return;
        }

        vtkPolyData * preCalcData = 
            precomputedPlanes->GetClosestPlane( requestedValue );

        if( preCalcData == NULL )
        {
            vprDEBUG( vesDBG, 0 )
            << "|\t\tcfdPresetVector: no precalculated data available"
            << std::endl << vprDEBUG_FLUSH;
            this->updateFlag = false;
            return;
        }
        //vtkPassThroughFilter* tempPipe = vtkPassThroughFilter::New();
        //tempPipe->SetInput( preCalcData );

        // get every nth point from the dataSet data
        this->ptmask->SetInput( preCalcData );
        this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
        this->ptmask->Update();

        this->SetGlyphWithThreshold();
        this->SetGlyphAttributes();
        this->glyph->Update();

        mapper->SetInputConnection( glyph->GetOutputPort() );
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( GetActiveDataSet()->
            GetActiveScalarName().c_str() );
        mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
        mapper->Update();
        
        //tempPipe->Delete();
        vprDEBUG( vesDBG, 1 ) << "|\t\tcfdPresetVector::Update Yes Precalc : "
            << this->cursorType << " : " << usePreCalcData
            << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
        if( xyz < 3 )
        {
            this->cuttingPlane = 
                new cfdCuttingPlane( GetActiveDataSet()->GetBounds(), 
                xyz, numSteps );
            // insure that we are using correct bounds for the given data set...
            this->cuttingPlane->SetBounds(
                                          this->GetActiveDataSet()->GetBounds() );
            this->cuttingPlane->Advance( requestedValue );
            vtkCutter* cutter = vtkCutter::New();
            cutter->SetInput( GetActiveDataSet()->GetDataSet() );
            cutter->SetCutFunction( this->cuttingPlane->GetPlane() );
            cutter->Update();
            delete this->cuttingPlane;
            this->cuttingPlane = NULL;
            c2p->SetInputConnection( cutter->GetOutputPort() );
            c2p->Update();
            cutter->Delete();
        }
        else if( xyz == 3 )
        {
            c2p->SetInput( GetActiveDataSet()->GetDataSet() );
            c2p->Update();
        }

        // get every nth point from the dataSet data
        this->ptmask->SetInputConnection( ApplyGeometryFilterNew( c2p->GetOutputPort() ) );
        this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
        this->ptmask->Update();


        this->SetGlyphWithThreshold();
        this->SetGlyphAttributes();
        //this->glyph->Update();

        /*{
            vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
            writer->SetInput( glyph->GetOutput() );
            writer->SetDataModeToAscii();
            writer->SetFileName( "testvecglyphs.vtp" );
            writer->Write();
            writer->Delete();
        }*/        
        
        mapper->SetInputConnection( glyph->GetOutputPort() );
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( GetActiveDataSet()->
            GetActiveScalarName().c_str() );
        mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
        mapper->Update();


        c2p->Delete();
        vprDEBUG( vesDBG, 1 )
            << "|\t\tNo Precalc : " << this->cursorType << " : " << usePreCalcData
            << " : " << GetVectorRatioFactor() << std::endl << vprDEBUG_FLUSH;
    }


    vtkActor* temp = vtkActor::New();
    temp->SetMapper( this->mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
   
	try
	{
		osg::ref_ptr<ves::xplorer::scenegraph::Geode > tempGeode = new ves::xplorer::scenegraph::Geode();

		if( gpustuff )
		{ 
            OSGStage osgStage;
			osgStage.createInstanced( this->ptmask->GetOutput(), 
                std::string(""), std::string(""), tempGeode.get() );
		}
		else
		{
			tempGeode->TranslateToGeode( temp );
		}

		geodes.push_back( tempGeode.get() );
		this->updateFlag = true;
	}
	catch( std::bad_alloc )
	{
		mapper->Delete();
		mapper = vtkPolyDataMapper::New();
		vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdPresetVectors "
            << std::endl << vprDEBUG_FLUSH;
	}
    temp->Delete();
}

