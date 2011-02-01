/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef VE_XPLORER_MODELHANDLER_H
#define VE_XPLORER_MODELHANDLER_H

#include <ves/VEConfig.h>
#include <ves/xplorer/ModelHandlerPtr.h>

#include <ves/xplorer/ModelPtr.h>
#include <ves/xplorer/DataSetPtr.h>
#include <ves/xplorer/event/EventHandlerPtr.h>

#include <ves/xplorer/event/cad/CADSlotInitializerPtr.h>

#include <boost/signals2/signal.hpp>

#include <ves/open/xml/CommandPtr.h>

#include <vpr/Util/Singleton.h>
#include <vpr/Sync/Mutex.h>

#include <osg/Node>

#include <vector>
#include <string>
#include <map>
#include <utility>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class CADEntity;
}
}
}

namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdTextureDataSet;
}
}
}
class vtkPolyData;


namespace ves
{
namespace xplorer
{
/*!\file ModelHandler.h
ModelHandler API
*/
/*!\class ves::xplorer::ModelHandler
*
*/
class VE_XPLORER_EXPORTS ModelHandler
{
private:
    // Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< ModelHandler >;
    //ModelHandler(const ModelHandler& o) { ; }
    //ModelHandler& operator=(const ModelHandler& o) { ; }
    ModelHandler();
    ~ModelHandler();
    vprSingletonHeader( ModelHandler );
public:
    //void CleanUp( void );
    void InitScene();
    void PreFrameUpdate();
    ///This is called in context predraw when there is a valid context available
    void ContextPreDrawUpdate();

    vtkPolyData* GetArrow();
    Model* GetModel( int );
    ///Add a new model
    void AddModel( Model* const );
    ///Remove a model
    void RemoveModel( Model* const );
    Model* GetActiveModel();
    ///Set the active model via uuid
    void SetActiveModel( std::string const& modelNumber );
    int GetNumberOfModels();

    void ReadNNumberOfDataSets( std::string, std::string );

    ///Register CAD file with so that other models can copy files if needed
    ///\param tempEntity File to be registered
    void RegisterCADFile( ves::xplorer::scenegraph::CADEntity* const tempEntity );
    ///Register a CAD file for the purposes of controlling animation overlay
    ///with dynamics data
    ///\param tempEntity File that is registered for viewing CAD data
    void RegisterAnimatedCADFile( ves::xplorer::scenegraph::CADEntity* const tempEntity );
    ///Check and see if this cad file is already loaded
    ///\return The found cad file otherwise null
    ///\param filename The file name to grab
    ves::xplorer::scenegraph::CADEntity* IsCADFileLoaded( std::string const& filename );
    ///Reset the map holding cad references
    ///\param tempEntity CAD file to try and remove from the map
    void UnregisterCADFile( ves::xplorer::scenegraph::CADEntity* const tempEntity );
    //texture manager access
    ///Should this call be in the texture manager singleton not modelhandler
    ves::xplorer::volume::cfdTextureDataSet* GetActiveTextureDataSet();
    ///Get whether tbase is enabled
    bool GetVisOption();
private:
    ///Create the arrow polydata
    vtkPolyData* _GetArrowPolyData();
    ///The current active dataset
    DataSet* activeDataset;
    ///The current command
    ves::open::xml::CommandPtr activeCommand;
    ///THe active model
    Model* _activeModel;
    ///Active texture based dataset
    ves::xplorer::volume::cfdTextureDataSet* _activeTDSet;
    ///Is this texture based
    bool tbased;
    ///The pd arrow for datasets
    vtkPolyData* arrow;
    ///The list of models being maintained
    std::vector< Model* > _modelList;
    // Used to store data for multi-dataset functions
    //std::string oldDatasetName;
    ///This flag tells the cad handler to rescale the textures of the files
    ///that are loaded.
    bool m_rescaleCADEntityTextures;
    
    ///The event handler for commands.
    std::map< std::string, ves::xplorer::event::EventHandler* > _eventHandlers;
    ///This map connects filenames to GUIDs so that we can
    ///figure out what CAD files should be copied
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const > m_filenameToCADMap;
    ///This map connects filenames to CADEntitys that have animation
    ///data attached so that we can control starting and stopping the animation
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const > m_animationCADMap;
    ///A mutex to protect variables accesses
    vpr::Mutex mValueLock;

    typedef boost::signals2::signal<void ( const std::string& )> ActiveModelChangedSignal_type;
    ActiveModelChangedSignal_type mActiveModelChangedSignal;
    ///Slot for initializing CAD
    ves::xplorer::event::cad::CADSlotInitializerPtr m_CADSlotInitializer;
};
}
}
#endif
