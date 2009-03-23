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
#ifndef VES_XPLORER_PLUGIN_BASE_H
#define VES_XPLORER_PLUGIN_BASE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <ves/xplorer/scenegraph/DCS.h>

#ifdef VE_SOUND
// --- osgAL Includes --- //
namespace osgAL
{
class SoundManager;
}
#endif

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- C/C++ Libraries --- //
#include <string>
#include <vector>
#include <map>

namespace ves
{
namespace xplorer
{
class cfdCursor;
class Device;
class EnvironmentHandler;
class ModelHandler;
class Model;
class CommandHandler;

namespace scenegraph
{
class DCS;
class PhysicsSimulator;
class SceneManager;
class ResourceManager;
}

namespace plugin
{
/*!\file PluginBase.h
 * PluginBase API
 */

/*!\class ::PluginBase
 *
 */
class VE_GRAPHICALPLUGINS_EXPORTS PluginBase
{
public:
    ///Default constructor
    PluginBase();
    
    ///Destructor
    virtual ~PluginBase();

    ///Methods to do scene graph manipulations
    ///New methods may have to be added later
    virtual void AddSelfToSG();

    ///Viz feature for the devloper to define
    ///Can be anything that creates a geode
    virtual void CreateCustomVizFeature( int input );
    
    ///Pass in the scene graph node that this plugin should be adding
    ///data to.
    ///\param veworldDCS The DCS that his plugin will add data to
    virtual void InitializeNode( osg::Group* veworldDCS );
    
    ///This gets called every frame no matter what
    //Allows graphical plugins access to scenegraph
    virtual void PreFrameUpdate();

    ///Allow the users to process new inputs after a job has
    ///been submitted for all plugins
    virtual void ProcessOnSubmitJob();

    ///Remove this plugin from the scenegraph
    virtual void RemoveSelfFromSG();

    ///This function gets called if the model is selected
    ///Allows graphical plugins access to scenegraph
    virtual void SelectedPreFrameUpdate();
    ///Tell the cfdExecutive whether this plugin is on the scenegraph
    ///\return Return true if this plugin is on the scenegraph
    bool OnSceneGraph();

    ///Return map that maps command names to this plugin
    ///\return The map used by cfdExecutive to process commands send by the gui
    std::map< std::string, PluginBase* > GetCommandNameMap();
    
    ///Get the model for this plugin
    ///\return The model for this plugin
    ves::xplorer::Model* GetCFDModel();

    ///Implement Gengxun's work by using socket
    ///stuff from vtk. This will be used in parallel
    ///with implementation of a unit connected to the
    ///computational engine.
    virtual void GetDataFromUnit();

    ///This returns the description of the module, This should be a short description
    ///\return The description of this plugin in string form
    const std::string& GetDesc();

    ///This returns the name of the module
    ///\return The name of the module in string form
    const std::string& GetName();
    
    ///Set current command whatever it is
    ///\param command Current command from conductor
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

    ///Set the pointer to the cursor class so that dynamic
    ///objects can do custom features with the wand input
    ///\param cursor The cursor from xplorer
    void SetCursor( ves::xplorer::cfdCursor* cursor );

    ///Set the pointer to the navigate class so that dynamic
    ///objects can do custom features with the wand buttons
    ///\param device The current active device
    void SetInteractionDevice( ves::xplorer::Device* device );

    ///Set the results for a particluar module so that we can use
    ///them for custom viz features
    ///\param input The module results in xml form
    void SetModuleResults( const std::string& input );

    ///Set the module name for this plugin
    ///\param input The module name
    void SetObjectName( const std::string& intput );
    
    ///Provide access to the physics simulator in the plugins
    ///\param physicsSimulator The physics simulator
    void SetPhysicsSimulator( ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );
    
    ///Provide access to the scene manager in the plugins
    ///\param sceneManager The scene manager
    void SetSceneManager( ves::xplorer::scenegraph::SceneManager* sceneManager );
    
    ///Provide access to the environment handler in the plugins
    ///\param environment The environment handler
    void SetEnvironmentHandler( ves::xplorer::EnvironmentHandler* environmentHandler );

    ///Provide access to the model handler in the plugins
    ///\param modelHandler The model handler
    void SetModelHandler( ves::xplorer::ModelHandler* modelHandler );

    ///Provide the resource manager instance for plugins
    ///\param resourceManager The resource manager
    void SetResourceManager( ves::xplorer::scenegraph::ResourceManager* resourceManager );

    ///Provide the conductor communication manager for plugins
    ///\param conductorComm The conductor CORBA pointer
    void SetCommandHandler( ves::xplorer::CommandHandler* commandHandler ); 
    
#ifdef VE_SOUND
    void SetSoundManager( osgAL::SoundManager* soundManager );
#endif

    ///Set the VE_Model to be used by this plugin
    ///\param tempModel Pointer to VE_Model
    void SetXMLModel( ves::open::xml::model::ModelPtr& tempModel );

    ///Get the DCS for this plugin
    ///\return The DCS that the CAD and viz are added to
    ves::xplorer::scenegraph::DCS* GetPluginDCS();
    
protected:
    ///Easy check to see if this plugin is on the scene graph
    bool mOnSceneGraph;
    ///This needs to match the name of the gui plugin.
    ///It is used in cfdVEPluginLoader::CreateObject. The name used is the name
    ///in the ves::open::xml::Model model name. This name comes from the gui
    ///plugin.
    std::string mObjectName;
    
    ves::xplorer::cfdCursor* mCursor;
    ves::xplorer::Device* mDevice;
    ///The model handler pointer
    ves::xplorer::ModelHandler* mModelHandler;
    ///The model for this plugin
    ves::xplorer::Model* mModel;
    ///The scene manager pointer
    ves::xplorer::scenegraph::SceneManager* mSceneManager;
    ///The environment handler pointer
    ves::xplorer::EnvironmentHandler* mEnvironmentHandler;
    ///The resource manager pointer
    ves::xplorer::scenegraph::ResourceManager* mResourceManager;
    ///The command handler pointer
    ves::xplorer::CommandHandler* mCommandHandler;
    ///The xml model pointer for this plugin
    ves::open::xml::model::ModelPtr mXmlModel;
    ///Singleton pointers
    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;
#ifdef VE_SOUND
    osgAL::SoundManager* mSoundManager;
#endif

    ///This is the base DCS pointer that all content for a plugin
    ///should be added to. 
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mDCS;
    ///This is the actuall parent DCS for this plugin. Typcially this is
    ///the Executive node that all plugins are added to.
    osg::ref_ptr< osg::Group > mWorldDCS;
    ///This map is populated with this pointers and with event names
    ///so that this plugin can execute events from conductor plugins
    std::map< std::string, PluginBase* > mEventHandlerMap;

private:
    std::string mNetwork;
    std::string mObjectDescription;
};
}
}
}

#define CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( name ) \
    extern "C" \
    { \
        VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin() \
        { \
            return new name(); \
        } \
    }

#endif //end VES_XPLORER_PLUGIN_BASE_H
