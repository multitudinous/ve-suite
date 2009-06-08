
///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Model.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_MODEL_WRAPPER_H
#define VES_XPLORER_MINERVA_MODEL_WRAPPER_H

#include <ves/VEConfig.h>

#include <Minerva/Core/Data/Model.h>
#include <Minerva/Interfaces/IElevationChangedListener.h>

namespace ves { namespace xplorer { namespace scenegraph { class CADEntity; } } }

namespace ves {
namespace xplorer {
namespace minerva {


class VE_XPLORER_EXPORTS ModelWrapper : 
  public Minerva::Core::Data::Model,
  public Minerva::Interfaces::IElevationChangedListener
{
public:

  typedef Minerva::Core::Data::Model BaseClass;
  typedef ves::xplorer::scenegraph::CADEntity CADEntity;
  typedef BaseClass::Extents Extents;

  USUL_DECLARE_QUERY_POINTERS ( ModelWrapper );
  USUL_DECLARE_IUNKNOWN_MEMBERS;

  ModelWrapper();

  /// Elevation has changed within given extents (IElevationChangeListener).
  virtual bool elevationChangedNotify ( 
    const Extents& extents, 
    unsigned int level, 
    ElevationDataPtr elevationData, 
    Usul::Interfaces::IUnknown * caller = 0x0 );

  /// Set/get the cad entity.
  void SetCADEntity ( CADEntity * );
  CADEntity* GetCADEntity() const;

  void UpdateMatrix ( Usul::Interfaces::IPlanetCoordinates* planet, Usul::Interfaces::IElevationDatabase* elevation );

protected:

  virtual ~ModelWrapper();

private:

  CADEntity *_cadEntity;

};


}
}
}


#endif // VES_XPLORER_MINERVA_MODEL_WRAPPER_H
