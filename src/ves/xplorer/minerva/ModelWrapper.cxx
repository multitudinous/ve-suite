
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
