
///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Minerva library.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/EventHandler.h>
#include <ves/xplorer/minerva/AddEarthHandler.h>
#include <ves/xplorer/minerva/AddElevationLayerHandler.h>
#include <ves/xplorer/minerva/AddRasterLayerHandler.h>
#include <ves/xplorer/minerva/RemoveEarthHandler.h>
#include <ves/xplorer/minerva/RemoveElevationLayerHandler.h>
#include <ves/xplorer/minerva/RemoveRasterLayerHandler.h>
#include <ves/xplorer/minerva/PropertiesHandler.h>
#include <ves/xplorer/minerva/TransformHandler.h>
#include <ves/xplorer/minerva/NavigateToModel.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <Minerva/Core/Data/Camera.h>
#include <Minerva/Core/TileEngine/Body.h>
#include <Minerva/Core/Functions/MakeBody.h>
#include <Minerva/Core/Layers/RasterLayerWms.h>

#include <OsgTools/Convert/Matrix.h>

#include <Usul/App/Application.h>
#include <Usul/Components/Manager.h>
#include <Usul/Errors/Assert.h>
#include <Usul/Pointers/Functions.h>
#include <Usul/Jobs/Manager.h>

#include <ves/util/commands/Minerva.h>
#include <ves/open/xml/Command.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

using namespace ves::xplorer::minerva;


vprSingletonImp ( MinervaManager );


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

MinervaManager::MinervaManager() : 
  _eventHandlers(),
  _currentCommand ( CommandPtr() ),
  _body ( 0x0 ),
  _manager ( 0x0 ),
  _scene ( 0x0 ),
  _models()
{
  // Set the program name since the cache is based on this.
  Usul::App::Application::instance().program ( "Minerva" );

  _eventHandlers[ves::util::commands::ADD_EARTH_COMMAND_NAME] = new AddEarthHandler;
  _eventHandlers[ves::util::commands::REMOVE_EARTH_COMMAND_NAME] = new RemoveEarthHandler;
  _eventHandlers[ves::util::commands::SET_GEOGRAPHIC_PROPERTIERS] = new PropertiesHandler;
  _eventHandlers["CAD_TRANSFORM_UPDATE"] = new TransformHandler;
  //_eventHandlers["CAD_DELETE_NODE"] = new DeleteHandler;
  _eventHandlers["Move to cad"] = new NavigateToModel;
  _eventHandlers[ves::util::commands::ADD_ELEVATION_LAYER] = new AddElevationLayerHandler;
  _eventHandlers[ves::util::commands::ADD_RASTER_LAYER] = new AddRasterLayerHandler;
  _eventHandlers[ves::util::commands::REMOVE_ELEVATION_LAYER] = new RemoveElevationLayerHandler;
  _eventHandlers[ves::util::commands::REMOVE_RASTER_LAYER] = new RemoveRasterLayerHandler;

#ifdef __APPLE__
  Usul::Components::Manager::instance().load ( Usul::Interfaces::IUnknown::IID, "GDALReadImage.plug" );
#endif
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

MinervaManager::~MinervaManager()
{
  // Delete the event handlers.
  EventHandlers::iterator iter ( _eventHandlers.begin() );
  while ( iter != _eventHandlers.end() )
  {
      delete iter->second;
      _eventHandlers.erase( iter++ );
  }

  for ( Models::iterator iter = _models.begin(); iter != _models.end(); ++iter )
  {
    Usul::Pointers::unreference ( iter->second );
    iter->second = 0x0;
  }
  _models.clear();

  this->Clear();
}


///////////////////////////////////////////////////////////////////////////////
//
//  Handle updates.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::PreFrameUpdate()
{
    vprDEBUG( vesDBG, 3 ) << "|MinervaManager::PreFrameUpdate" 
    << std::endl << vprDEBUG_FLUSH;
    ves::open::xml::CommandPtr tempCommand =
        ves::xplorer::ModelHandler::instance()->GetXMLCommand();
  if ( tempCommand )
  {
    const std::string name ( tempCommand->GetCommandName() );

    EventHandlers::iterator iter ( _eventHandlers.find ( name ) );
    if ( iter != _eventHandlers.end() )
    {
      EventHandler *handler ( iter->second );
      if ( 0x0 != handler )
      {
        vprDEBUG( vesDBG, 0 ) << "|Minerva manager executing: " << name << std::endl << vprDEBUG_FLUSH;
        handler->Execute ( tempCommand, *this );
      }
    }

    // Clear the command.
    //_currentCommand = CommandPtr();
  }

  if ( _body )
  {
    // Remove all tiles that are ready for deletion.
    _body->purgeTiles();
  }
    vprDEBUG( vesDBG, 3 ) << "|MinervaManager::PreFrameUpdate End" 
    << std::endl << vprDEBUG_FLUSH;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Set the command.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::SetVECommand ( CommandPtr command )
{
  _currentCommand = command;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add Earth to the scene.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddEarthToScene()
{
  // Make sure.
  this->Clear();

  _manager = new Usul::Jobs::Manager ( "VE-Suite Minerva Job Manager", 4 );
  _body = Minerva::Core::Functions::makeEarth ( _manager );
  Usul::Pointers::reference ( _body );

  // Until a user interface is in place to add layers, add some base layers.
  {
#if 0
    typedef Minerva::Core::Layers::RasterLayerWms RasterLayerWms;
    typedef RasterLayerWms::Options Options;
    typedef RasterLayerWms::Extents Extents;

    Extents extents ( -180.0, -90.0, 180.0, 90.0 );
    Options options;

    // Add srtm for terrain.
    options["format"] = "image/tiff";
    options["layers"] = "strmplus";
    options["request"] = "GetMap";
    options["service"] = "WMS";
    options["srs"] = "EPSG:4326";
    options["styles"] = "";
    options["version"] = "1.1.1";

    RasterLayerWms::RefPtr srtm ( new RasterLayerWms ( extents, "http://serv.asu.edu/cgi-bin/srtm.cgi", options ) );
    _body->elevationAppend ( Usul::Interfaces::IRasterLayer::QueryPtr ( srtm ) );
#endif

  }

  _scene = _body->scene();

  osg::ref_ptr<osg::Group> root ( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
  if ( root.valid() )
  {
    root->addChild ( _scene.get() );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Clear.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::Clear()
{
    osg::ref_ptr<osg::Group> root( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
    if( root.valid() )
    {
        root->removeChild( _scene.get() );
    }
    _scene = 0x0;

    if( 0x0 != _body )
    {
        _body->clear();
        Usul::Pointers::unreference( _body );
        _body = 0x0;
    }

    // Clean up job manager.
    if( 0x0 != _manager )
    {
        // Remove all queued jobs and cancel running jobs.
        _manager->cancel();

        // Wait for remaining jobs to finish.
        _manager->wait();

        // Delete the manager.
        delete _manager;
        _manager = 0x0;
    }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add the model.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddModel ( const std::string& guid, ModelWrapper* model )
{
  if ( 0x0 != _body )
  {
    _body->vectorData()->add ( Usul::Interfaces::IUnknown::QueryPtr ( model ) );
    
    // Unreference anything we may have.
    Usul::Pointers::unreference ( _models[guid] );
    
    // Add the model to our map.
    Usul::Pointers::reference ( model );
    _models[guid] = model;

    this->UpdateModel ( model );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Get the model wrapper for the guid.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper* MinervaManager::GetModel ( const std::string& guid ) const
{
  Models::const_iterator iter ( _models.find ( guid ) );
  return ( iter != _models.end() ? iter->second : 0x0 );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Do we have a model already for the guid?
//
///////////////////////////////////////////////////////////////////////////////

bool MinervaManager::HasModel ( const std::string& guid ) const
{
  Models::const_iterator iter ( _models.find ( guid ) );
  return iter != _models.end();
}


///////////////////////////////////////////////////////////////////////////////
//
//  Update the matrix for the model.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::UpdateModel ( ModelWrapper* model )
{
  if ( 0x0 != model && 0x0 != _body )
  {
    Usul::Interfaces::IPlanetCoordinates::QueryPtr planet ( _body );
    Usul::Interfaces::IElevationDatabase::QueryPtr elevation ( _body );
    model->UpdateMatrix ( planet.get(), elevation.get() );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Delete the model.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::RemoveModel ( const std::string& guid )
{
  ModelWrapper::RefPtr wrapper ( this->GetModel ( guid ) );
  if ( wrapper.valid() )
  {
    if ( 0x0 != _body )
    {
      _body->vectorData()->remove ( Usul::Interfaces::IUnknown::QueryPtr ( wrapper ) );
    }

    _models.erase ( guid );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Create the view matrix for the given camera.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::GetViewMatrix ( Minerva::Core::Data::Camera* camera, osg::Matrix& matrix ) const
{
  if ( 0x0 == _body || 0x0 == camera )
    return;

  Minerva::Core::TileEngine::LandModel::RefPtr landModel ( _body->landModel() );
  if ( !landModel.valid() )
    return;

  typedef Minerva::Core::Data::Camera::Matrix Matrix;
  Matrix m ( camera->viewMatrix ( landModel.get() ) );

  Usul::Convert::Type<Matrix,osg::Matrixd>::convert ( m, matrix );
  matrix = osg::Matrixd::inverse ( matrix );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add an elevation layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddElevationLayer ( Minerva::Core::Layers::RasterLayer* layer )
{
  if ( 0x0 == _body || 0x0 == layer )
    return;

  _body->elevationAppend ( Usul::Interfaces::IRasterLayer::QueryPtr ( layer ) );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddRasterLayer ( Minerva::Core::Layers::RasterLayer* layer )
{
  if ( 0x0 == _body || 0x0 == layer )
    return;

  _body->rasterAppend ( Usul::Interfaces::IRasterLayer::QueryPtr ( layer ) );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove an elevation layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::RemoveElevationLayer ( const std::string& guid )
{
  if ( 0x0 == _body )
    return;

  // This dynamic cast should always be true.
  typedef Minerva::Core::TileEngine::Body::RasterGroup RasterGroup;
  RasterGroup::RefPtr group ( dynamic_cast<RasterGroup*> ( _body->elevationData().get() ) );
  Extents extents;
  this->_removeLayer ( group.get(), guid, extents );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::RemoveRasterLayer ( const std::string& guid )
{
  if ( 0x0 == _body )
    return;

  // This dynamic cast should always be true.
  typedef Minerva::Core::TileEngine::Body::RasterGroup RasterGroup;
  RasterGroup::RefPtr group ( dynamic_cast<RasterGroup*> ( _body->rasterData().get() ) );

  Extents extents;
  if ( this->_removeLayer ( group.get(), guid, extents ) )
  {
    _body->dirtyTextures ( extents );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove a layer from the group.
//
///////////////////////////////////////////////////////////////////////////////

bool MinervaManager::_removeLayer ( Minerva::Core::Layers::RasterGroup *group, const std::string& guid, Extents& extents )
{
    typedef Minerva::Core::TileEngine::Body::RasterGroup RasterGroup;
    typedef Minerva::Core::TileEngine::Body::RasterLayer RasterLayer;
    typedef RasterGroup::Layers Layers;
    typedef RasterGroup::IRasterLayer IRasterLayer;

    if( 0x0 != group )
    {
        // The api doesn't support removing by a guid, so this is a little ugly.
        IRasterLayer::RefPtr layerToRemove ( 0x0 );

        // Loop through all the layers and look for a layer that matches our guid.
        Layers layers;
        group->layers ( layers );
        for ( Layers::iterator iter = layers.begin(); iter != layers.end(); ++iter )
        {
            Usul::Interfaces::ILayer::QueryPtr layer ( *iter );
            if ( layer.valid() && guid == layer->guid() )
            {
                layerToRemove = *iter;
            }
        }

        // If we found a layer, remove it.
        if ( layerToRemove.valid() )
        {
            group->remove ( layerToRemove.get() );

            Usul::Interfaces::ILayerExtents::QueryPtr le ( layerToRemove );
            const double minLon ( le.valid() ? le->minLon() : -180.0 );
            const double minLat ( le.valid() ? le->minLat() : -90.0 );
            const double maxLon ( le.valid() ? le->maxLon() : 180.0 );
            const double maxLat ( le.valid() ? le->maxLat() : 90.0 );
            extents = Extents ( minLon, minLat, maxLon, maxLat );

            return true;
        }
    }

    return false;
}
