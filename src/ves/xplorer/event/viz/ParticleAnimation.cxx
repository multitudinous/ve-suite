/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include <ves/xplorer/event/viz/ParticleAnimation.h>
#include <ves/xplorer/event/viz/OSGParticleStage.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkTubeFilter.h>
#include <vtkCellTypes.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkWarpVector.h>

// following is for vertex-based sphere glyphs
#include <vtkGlyph3D.h>
#include <vtkSphereSource.h>
#include <vtkPointData.h>
#include <vtkPolyDataNormals.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::event::viz;

ParticleAnimation::ParticleAnimation()
{
    this->map = vtkPolyDataMapper::New();
    this->map->SetColorModeToMapScalars();
    warper = vtkWarpVector::New();
    warpSurface = false;
    warpedContourScale = 1.0f;

    _particleOption = 0;
    _particleScale = 1;
    //this->map->ScalarVisibilityOff();
    /*
       this->actor->GetProperty()->SetColor( 1.0f, 1.0f, 1.0f );
       this->actor->GetProperty()->SetAmbient( 0.2f );
       this->actor->GetProperty()->SetDiffuse( 0.8f );
       this->actor->GetProperty()->SetSpecular( 0.3f );
       this->actor->GetProperty()->SetSpecularPower( 20.0f );
       this->actor->GetProperty()->SetOpacity( this->op );
    */
}

ParticleAnimation::~ParticleAnimation()
{
    this->map->Delete();
    warper->Delete();
}

void ParticleAnimation::Update()
{
    if( GetActiveDataSet() == NULL )
    {
        vprDEBUG( vesDBG, 0 )
            << "|\tParticleAnimation has no data so setting updateFlag to false"
            << std::endl << vprDEBUG_FLUSH;
        this->updateFlag = false;
        return;
    }
    else
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tParticleAnimation: this->GetActiveDataSet() = "
            << GetActiveDataSet()->GetFileName() << std::endl 
            << vprDEBUG_FLUSH;
    }

    if( m_gpuTools )
    {
        OSGParticleStage* particles = new OSGParticleStage();
        geodes.push_back( particles->createInstanced( 
            GetActiveDataSet()->GetTransientDataSets(),
            GetActiveDataSet()->GetActiveScalarName(), 
            GetActiveDataSet()->GetActiveVectorName() ) );
        delete particles;
        updateFlag = true;
    }
}

//////////////////////////////////////////////////////////
void ParticleAnimation::SetParticleOption( unsigned int option )
{
    _particleOption = option;
}
//////////////////////////////////////////////
unsigned int ParticleAnimation::GetParticleOption()
{
    return _particleOption;
}
//////////////////////////////////////////////
void ParticleAnimation::SetParticleScale( float x )
{
    _particleScale = x;
}
/////////////////////////////////////
float ParticleAnimation::GetParticleScale()
{
    return _particleScale;
}
/////////////////////////////////
void ParticleAnimation::UpdateCommand()
{
//   cfdObjects::UpdateCommand();
//   std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;

    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP =
        veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = 
        boost::dynamic_pointer_cast<ves::open::xml::Command>(  
        activeModelDVP->GetDataXMLObject() );

    //Extract the isosurface value
    activeModelDVP = objectCommand->GetDataValuePair( "Polydata Value" );
    //double planePosition;
    activeModelDVP->GetData( warpedContourScale );
    //SetRequestedValue( static_cast< int >( planePosition ) );

    activeModelDVP = objectCommand->GetDataValuePair( "Color By Scalar" );
    activeModelDVP->GetData( colorByScalar );

    activeModelDVP = objectCommand->GetDataValuePair( "Warped Surface" );
    unsigned int surface;
    activeModelDVP->GetData( surface );
    if( surface == 1 )
    {
        warpSurface = true;
    }
    else if( surface == 0 )
    {
        warpSurface = false;
    }

    activeModelDVP = objectCommand->GetDataValuePair( "GPU Tools" );
    unsigned int gpuTools;
    activeModelDVP->GetData( gpuTools );
    m_gpuTools = gpuTools;
}

float ParticleAnimation::GetSphereScaleFactor()
{
    // this->GetParticleScale() is obtained from gui, -100 < sphereScale < 100
    // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
    // convert range to -4 < x < 4, and compute the exponent...
    vprDEBUG( vesDBG, 1 ) 
        << "|\t\tParticleAnimation::GetSphereScaleFactor sphereScale = " 
        << GetParticleScale()
        << std::endl << vprDEBUG_FLUSH;

    float scaleFactor = 0.0;

    if( -100 <= this->GetParticleScale() && this->GetParticleScale() <= 100 )
    {
        scaleFactor = exp( this->GetParticleScale() / 25.0 );
    }

    vprDEBUG( vesDBG, 1 ) 
        << "\t\tParticleAnimation::GetSphereScaleFactor scaleFactor = " 
        << scaleFactor
        << std::endl << vprDEBUG_FLUSH;

    return scaleFactor;
}

