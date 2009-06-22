
///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Minerva library.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/EventHandler.h>
#include <ves/xplorer/minerva/AddEarthHandler.h>
#include <ves/xplorer/minerva/RemoveEarthHandler.h>
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
  if ( _currentCommand )
  {
    const std::string name ( _currentCommand->GetCommandName() );

    EventHandlers::iterator iter ( _eventHandlers.find ( name ) );
    if ( iter != _eventHandlers.end() )
    {
      EventHandler *handler ( iter->second );
      if ( 0x0 != handler )
      {
        vprDEBUG( vesDBG, 0 ) << "|Minerva manager executing: " << name << std::endl << vprDEBUG_FLUSH;
        handler->Execute ( _currentCommand, *this );
      }
    }

    // Clear the command.
    _currentCommand = CommandPtr();
  }

  if ( _body )
  {
    // Remove all tiles that are ready for deletion.
    _body->purgeTiles();
  }
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

    // Add OpenAerialMap for raster.
    options.clear();
    options["format"] = "image/jpeg";
    options["layers"] = "OpenAerialMap";
    options["request"] = "GetMap";
    options["service"] = "WMS";
    options["srs"] = "EPSG:4326";
    options["styles"] = "";
    options["version"] = "1.1.1";

    RasterLayerWms::RefPtr openAerialMap ( new RasterLayerWms ( extents, "http://serv.asu.edu/cgi-bin/tilecache-2.03/tilecache.cgi", options ) );
    _body->rasterAppend ( Usul::Interfaces::IRasterLayer::QueryPtr ( openAerialMap ) );
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
  osg::ref_ptr<osg::Group> root ( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
  if ( root.valid() )
  {
    root->removeChild ( _scene.get() );
  }
  _scene = 0x0;

  if ( 0x0 != _body )
  {
    _body->clear();
    Usul::Pointers::unreference ( _body );
    _body = 0x0;
  }

  // Clean up job manager.
  if ( 0x0 != _manager )
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
