/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
class Model;

namespace scenegraph
{
class DCS;
class PhysicsSimulator;
class SceneManager;
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
    PluginBase();

    virtual ~PluginBase();

    //Methods to do scene graph manipulations
    //New methods may have to be added later
    virtual void AddSelfToSG();

    //Viz feature for the devloper to define
    //Can be anything that creates a geode
    virtual void CreateCustomVizFeature( int input );

    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS );

    ///This gets called every frame no matter what
    //Allows graphical plugins access to scenegraph
    virtual void PreFrameUpdate();

    ///Allow the users to process new inputs after a job has
    ///been submitted for all plugins
    virtual void ProcessOnSubmitJob();

    virtual void RemoveSelfFromSG();

    ///This function gets called if the model is selected
    //Allows graphical plugins access to scenegraph
    virtual void SelectedPreFrameUpdate();

    bool OnSceneGraph();

    ///Return map that maps command names to this plugin
    std::map< std::string, PluginBase* > GetCommandNameMap();

    ves::xplorer::Model* GetCFDModel();

    //Implement Gengxun's work by using socket
    //stuff from vtk. This will be used in parallel
    //with implementation of a unit connected to the
    //computational engine.
    virtual void GetDataFromUnit();

    //This returns the description of the module, This should be a short description
    const std::string& GetDesc();

    //This returns the name of the module
    const std::string& GetName();
    
    ///Set current command whatever it is
    ///\param command Current command from conductor
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

    //Set the pointer to the cursor class so that dynamic
    //objects can do custom features with the wand input
    void SetCursor( ves::xplorer::cfdCursor* cursor );

    //Set the id for a particular module
    void SetID( int id );

    //Set the pointer to the navigate class so that dynamic
    //objects can do custom features with the wand buttons
    void SetInteractionDevice( ves::xplorer::Device* device );

    //Set the results for a particluar module so that we can use
    //them for custom viz features
    void SetModuleResults( const std::string& input );

    void SetObjectName( const std::string& intput );
    ///Provide access to the physics simulator in the plugins
    ///\param physicsSimulator The physics simulator
    void SetPhysicsSimulator( ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );
    ///Provide access to the scene manager in the plugins
    ///\param sceneManager The scene manager
    void SetSceneManager( ves::xplorer::scenegraph::SceneManager* sceneManager );
    ///Provide access to the environment handler in the plugins
    ///\param environment The environment handler
    void SetEnvironmentHandler( ves::xplorer::EnvironmentHandler* environment );
    
#ifdef VE_SOUND
    void SetSoundManager( osgAL::SoundManager* soundManager );
#endif

    //transform object based
    void SetTransforms( double* scale, double* rot, double* trans );

    ///Set the VE_Model to be used by this plugin
    ///\param tempModel Pointer to VE_Model
    void SetXMLModel( ves::open::xml::model::ModelPtr& tempModel );

protected:
    bool mOnSceneGraph;

    long mPosX;
    long mPosY;

    int mModelID;

    std::string mObjectName;
    
    ves::xplorer::cfdCursor* mCursor;
    ves::xplorer::Device* mDevice;
    ///The model for this plugin
    ves::xplorer::Model* mModel;
    ///The scene manager pointer
    ves::xplorer::scenegraph::SceneManager* mSceneManager;
    ///The environment handler pointer
    ves::xplorer::EnvironmentHandler* mEnvironment;
    ///The xml model pointer for this plugin
    ves::open::xml::model::ModelPtr mXmlModel;

    //Singleton pointers
    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;
#ifdef VE_SOUND
    osgAL::SoundManager* mSoundManager;
#endif

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mWorldDCS;

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
