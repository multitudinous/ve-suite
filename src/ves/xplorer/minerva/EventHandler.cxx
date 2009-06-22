
///////////////////////////////////////////////////////////////////////////////
//
//  Base event handler for Minerva commands.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/EventHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

EventHandler::EventHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

EventHandler::~EventHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Get or create the model.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper* EventHandler::GetOrCreateModel ( const std::string& nodeId, MinervaManager& manager )
{
  ModelWrapper::RefPtr modelWrapper ( 0x0 );
  if ( manager.HasModel ( nodeId ) )
  {
    modelWrapper = manager.GetModel ( nodeId );
  }
  else
  {
    const unsigned int numModels ( ves::xplorer::ModelHandler::instance()->GetNumberOfModels() );
    for ( unsigned int i = 0; i < numModels; ++i )
    {
      ves::xplorer::Model *model ( ves::xplorer::ModelHandler::instance()->GetModel ( i ) );
      if ( 0x0 != model )
      {
        ves::xplorer::ModelCADHandler *modelHandler ( model->GetModelCADHandler() );
        if ( 0x0 != modelHandler )
        {
          ves::xplorer::scenegraph::CADEntity* tempPart ( modelHandler->GetPart ( nodeId ) );
          if ( 0x0 != tempPart )
          {
            modelWrapper = new ModelWrapper;

            // ves units are in feet.  Add the conversion to meters.
            modelWrapper->toMeters ( 0.3048 );
            modelWrapper->SetCADEntity ( tempPart );
            manager.AddModel ( nodeId, modelWrapper.get() );

            // For debugging placement.
#if 0
            modelWrapper->scale ( osg::Vec3d ( 100000.0, 100000.0, 100000.0 ) );
#endif

          }
        }
      }
    }
  }

  return modelWrapper.release();
}
