
///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Minerva library.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_MANAGER_H
#define VES_XPLORER_MINERVA_MANAGER_H

#include <ves/VEConfig.h>
#include <ves/open/xml/CommandPtr.h>

#include <vpr/Util/Singleton.h>

#include "osg/Node"
#include "osg/ref_ptr"

#include <string>
#include <map>

namespace Minerva { namespace Core { namespace TileEngine { class Body; } } }
namespace Minerva { namespace Core { namespace Data { class Camera; } } }
namespace Minerva { namespace Core { namespace Layers { class RasterLayer; class RasterGroup; } } }
namespace Minerva { namespace Core { template<class T> class Extents; } }
namespace Usul { namespace Jobs { class Manager; } }
namespace osg { class Matrixd; class osg::Vec2d; }

namespace ves {
namespace xplorer {
namespace minerva {

  class EventHandler;
  class ModelWrapper;

class VE_XPLORER_EXPORTS MinervaManager
{
public:

  typedef ves::open::xml::CommandPtr CommandPtr;
  typedef Minerva::Core::TileEngine::Body Body;
  typedef Minerva::Core::Extents<osg::Vec2d> Extents;

  void AddEarthToScene();
  void AddElevationLayer ( Minerva::Core::Layers::RasterLayer* );
  void AddRasterLayer ( Minerva::Core::Layers::RasterLayer* );
  void AddModel ( const std::string& guid, ModelWrapper* model );

  void Clear();

  ModelWrapper* GetModel ( const std::string& guid ) const;
  bool HasModel ( const std::string& guid ) const;

  void GetViewMatrix ( Minerva::Core::Data::Camera* camera, osg::Matrix& matrix ) const;

  void PreFrameUpdate();

  void RemoveElevationLayer ( const std::string& guid );
  void RemoveRasterLayer ( const std::string& guid );
  void RemoveModel ( const std::string& guid );

  void SetVECommand ( CommandPtr command );

  void UpdateModel ( ModelWrapper* );

private:

  bool _removeLayer ( Minerva::Core::Layers::RasterGroup *group, const std::string& guid, Extents& extents );

  MinervaManager();
  ~MinervaManager();

  /// Do not copy.
  MinervaManager ( const MinervaManager& );
  MinervaManager& operator= ( const MinervaManager& );

  vprSingletonHeader ( MinervaManager );

  typedef std::map<std::string,EventHandler*> EventHandlers;
  typedef std::map<std::string,ModelWrapper*> Models;

  EventHandlers _eventHandlers;
  CommandPtr _currentCommand;
  Body* _body;
  Usul::Jobs::Manager* _manager;
  osg::ref_ptr<osg::Node> _scene;
  Models _models; 
};


}
}
}

#endif // VES_XPLORER_MINERVA_MANAGER_H
