
///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Model.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/ModelWrapper.h>

#include <Usul/Interfaces/IPlanetCoordinates.h>
#include <Usul/Interfaces/IElevationDatabase.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

using namespace ves::xplorer::minerva;

USUL_IMPLEMENT_IUNKNOWN_MEMBERS ( ModelWrapper, ModelWrapper::BaseClass );


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper::ModelWrapper() : BaseClass(),
  _cadEntity ( 0x0 )
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper::~ModelWrapper()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Query for the interface.
//
///////////////////////////////////////////////////////////////////////////////

Usul::Interfaces::IUnknown* ModelWrapper::queryInterface ( unsigned long iid )
{
  switch ( iid )
  {
  case Minerva::Interfaces::IElevationChangedListener::IID:
    return static_cast<Minerva::Interfaces::IElevationChangedListener*> ( this );
  default:
    return BaseClass::queryInterface ( iid );
  }
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
    Usul::Interfaces::IPlanetCoordinates::QueryPtr planet ( caller );
    Usul::Interfaces::IElevationDatabase::QueryPtr elevation ( caller );
    this->UpdateMatrix ( planet.get(), elevation.get() );
    
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

void ModelWrapper::UpdateMatrix ( Usul::Interfaces::IPlanetCoordinates* planet, Usul::Interfaces::IElevationDatabase* elevation )
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
