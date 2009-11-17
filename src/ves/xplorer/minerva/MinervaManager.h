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
 * Date modified: $Date: 2009-06-28 23:47:14 -0700 (Sun, 28 Jun 2009) $
 * Version:       $Rev: 12939 $
 * Author:        $Author: akubach $
 * Id:            $Id: AppFrame.cxx 12939 2009-06-29 06:47:14Z akubach $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

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

#include <gmtl/Matrix.h>

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

  void GetViewMatrix ( Minerva::Core::Data::Camera* camera, gmtl::Matrix44d& matrix ) const;

  void PreFrameUpdate();

  void RemoveElevationLayer ( const std::string& guid );
  void RemoveRasterLayer ( const std::string& guid );
  void RemoveModel ( const std::string& guid );

  void SetVECommand ( CommandPtr command );

  void UpdateModel ( ModelWrapper* );

private:

  bool _removeLayer ( Minerva::Core::Layers::RasterGroup *group, const std::string& guid, Extents& extents );

  void _loadPlugins();
  void _unloadPlugins();

  MinervaManager();
  ~MinervaManager();

  /// Do not copy.
  //MinervaManager ( const MinervaManager& );
  //MinervaManager& operator= ( const MinervaManager& );

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
