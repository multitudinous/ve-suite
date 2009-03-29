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

#include <ves/open/xml/CommandPtr.h>

#include <vpr/Util/Singleton.h>

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
    ModelHandler( void );
    ~ModelHandler( void );
    vprSingletonHeader( ModelHandler );
public:
    void Initialize( std::string );
    //void CleanUp( void );
    void InitScene( void );
    void PreFrameUpdate( void );
    ///This is called in context predraw when there is a valid context available
    void ContextPreDrawUpdate();
    ///Set the active xml command
    ///\param inputCommand input xml command
    void SetXMLCommand( const ves::open::xml::CommandPtr& inputCommand );
    ///Returns the current xml command
    const ves::open::xml::CommandPtr& GetXMLCommand( void );

    vtkPolyData* GetArrow( void );
    Model* GetModel( int );
    void AddModel( Model* );
    void RemoveModel( Model* );
    Model* GetActiveModel( void );
    void SetActiveModel( const std::string& modelNumber );
    int GetNumberOfModels( void );

    void ReadNNumberOfDataSets( std::string, std::string );

    ///Get the scalar bar - may not be needed anymore
    //cfdScalarBarActor* GetScalarBar(void);
    ///Register CAD file with so that other models can copy files if needed
    ///\param tempEntity File to be registered
    void RegisterCADFile( ves::xplorer::scenegraph::CADEntity* tempEntity );
    ///Check and see if this cad file is already loaded
    ///\return The found cad file otherwise null
    ///\param filename The file name to grab
    ves::xplorer::scenegraph::CADEntity* IsCADFileLoaded( std::string filename );
    ///Reset the map holding cad references
    ///\param tempEntity CAD file to try and remove from the map
    void UnregisterCADFile( ves::xplorer::scenegraph::CADEntity* tempEntity );
    //texture manager access
    ///Should this call be in the texture manager singleton not modelhandler
    ves::xplorer::volume::cfdTextureDataSet* GetActiveTextureDataSet( void );
    bool GetVisOption();
protected:
    vtkPolyData* _GetArrowPolyData();
private:
    DataSet* activeDataset;
    ves::open::xml::CommandPtr activeCommand;
    ves::open::xml::CommandPtr nullCommand;
    Model* _activeModel;

    ves::xplorer::volume::cfdTextureDataSet* _activeTDSet;
    bool tbased;
    vtkPolyData* arrow;
    std::vector< Model* > _modelList;
    // Used to store data for multi-dataset functions
    //std::string oldDatasetName;
    ///This flag tells the cad handler to rescale the textures of the files
    ///that are loaded.
    bool m_rescaleCADEntityTextures;
    
    ///The event handler for commands.
    std::map< std::string, ves::xplorer::event::EventHandler*> _eventHandlers;
    ///This map connects filenames to GUIDs so that we can
    ///figure out what CAD files should be copied
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* > m_filenameToCADMap;
};
}
}
#endif
