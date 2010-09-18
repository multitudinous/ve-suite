/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Model.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

using namespace ves::xplorer::minerva;


typedef Minerva::Common::IPlanetCoordinates IPlanetCoordinates;
typedef Minerva::Common::IElevationDatabase IElevationDatabase;

///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper::ModelWrapper() : BaseClass(),
  _cadEntity ( 0x0 ),
  _offset ( 0.0, 0.0, 0.0 ),
  _parent ( 0x0 )
{
  // ves units are in feet.  Add the conversion to meters.
  this->toMeters ( 0.3048 );

  this->altitudeMode ( Minerva::Core::Data::ALTITUDE_MODE_RELATIVE_TO_GROUND );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper::~ModelWrapper()
{
  _cadEntity = 0x0;
  _parent = 0x0;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Elevation has changed within given extents (IElevationChangeListener).
//
///////////////////////////////////////////////////////////////////////////////

bool ModelWrapper::elevationChangedNotify ( 
  const Extents& extents, 
  unsigned int level, 
  ElevationDataPtr elevationData, 
  Usul::Interfaces::IUnknown * caller )
{
  BaseClass::Extents e ( this->extents() );

  if ( e.intersects ( extents ) )
  {
    Guard guard ( this->mutex() );

    osg::Vec3d location ( this->location() );
    osg::Vec3d tempLocation ( location );
    tempLocation[2] += _offset[2];

    this->location ( tempLocation );

    IPlanetCoordinates::QueryPtr planet ( caller );
    IElevationDatabase::QueryPtr elevation ( caller );
    this->UpdateMatrix ( planet.get(), elevation.get() );

    this->location ( location );

    return true;
  }

  return false;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Set the cad entity.
//
///////////////////////////////////////////////////////////////////////////////

void ModelWrapper::SetCADEntity ( CADEntity *entity )
{
  Guard guard ( this );
  _cadEntity = entity;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Get the cad entity.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper::CADEntity* ModelWrapper::GetCADEntity() const
{
  Guard guard ( this );
  return _cadEntity;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Update the matrix.
//
///////////////////////////////////////////////////////////////////////////////

void ModelWrapper::UpdateMatrix ( IPlanetCoordinates* planet, IElevationDatabase* elevation )
{
  Matrix matrix ( this->matrix ( planet, elevation ) );

  CADEntity *entity ( this->GetCADEntity() );
  if ( 0x0 != entity )
  {
    ves::xplorer::scenegraph::DCS* dcs ( entity->GetDCS() );
    if ( 0x0 != dcs )
    {
      gmtl::Matrix44d theMatrix;
      theMatrix.set ( matrix.ptr() );
      dcs->SetMat ( theMatrix );
    }
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Set the translation offset of cad in feet.
//
///////////////////////////////////////////////////////////////////////////////

void ModelWrapper::setTranslationOffset ( double x, double y, double z )
{
  Guard guard ( this->mutex() );
  _offset.set ( x, y, z );
}

// Set/get the parent.
void ModelWrapper::SetParent ( Minerva::Core::Data::DataObject* parent )
{
  _parent = parent;
}

Minerva::Core::Data::DataObject* ModelWrapper::GetParent() const
{
  return _parent;
}
