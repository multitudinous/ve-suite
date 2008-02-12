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
#ifndef CFD_VE_BASE_CLASS_H
#define CFD_VE_BASE_CLASS_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>

#include <ves/xplorer/ModelPtr.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/CommandPtr.h>

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
class cfdObjects;
class cfdSoundHandler;

namespace scenegraph
{
class DCS;
class Group;
class PhysicsSimulator;
}

namespace plugin
{
/*!\file cfdVEBaseClass.h
 * cfdVEBaseClass API
 */

/*!\class ::cfdVEBaseClass
 *
 */
class VE_GRAPHICALPLUGINS_EXPORTS cfdVEBaseClass
{
public:
    cfdVEBaseClass();

    virtual ~cfdVEBaseClass();

    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );

    //Methods to do scene graph manipulations
    //New methods may have to be added later
    virtual void AddSelfToSG();

    virtual void RemoveSelfFromSG();

    //transform object based
    void SetTransforms( double*, double*, double* );

    //Implement Gengxun's work by using socket
    //stuff from vtk. This will be used in parallel
    //with implementation of a unit connected to the
    //computational engine.
    virtual void GetDataFromUnit();

    //Basically uses vtkActorToPF to create a geode and
    //add it to the scene graph. Probably use cfdObject.
    virtual void MakeGeodeByUserRequest( int );

    //This returns the name of the module
    std::string GetName();

    //This returns the description of the module, This should be a short description
    std::string GetDesc();

    //Set the id for a particular module
    void SetID( int id );

    ves::xplorer::Model* GetCFDModel();

    bool OnSceneGraph()
    {
        return m_onSceneGraph;
    }

    //Set the pointer to the cursor class so that dynamic
    //objects can do custom features with the wand input
    void SetCursor( ves::xplorer::cfdCursor* );

    //Set the pointer to the navigate class so that dynamic
    //objects can do custom features with the wand buttons
    void SetInteractionDevice( ves::xplorer::Device* device );

    void SetSoundHandler( ves::xplorer::cfdSoundHandler* input );

    void SetPhysicsSimulator( ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

#ifdef VE_SOUND
    void SetSoundManager( osgAL::SoundManager* soundManager );
#endif

    //Set the results for a particluar module so that we can use
    //them for custom viz features
    void SetModuleResults( const std::string );

    void SetObjectName( std::string );

    //Viz feature for the devloper to define
    //Can be anything that creates a geode
    virtual void CreateCustomVizFeature( int );

    ///This function gets called if the model is selected
    virtual void SelectedPreFrameUpdate()
    {
        //Allows graphical plugins access to scenegraph
        ;
    }

    ///This gets called every frame no matter what
    virtual void PreFrameUpdate()
    {
        //Allows graphical plugins access to scenegraph
        ;
    }

    ///Set the VE_Model to be used by this plugin
    ///\param tempModel Pointer to VE_Model
    void SetXMLModel( ves::open::xml::model::ModelWeakPtr tempModel );

    ///Set current command whatever it is
    ///\param command Current command from conductor
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

    ///Allow the users to process new inputs after a job has
    ///been submitted for all plugins
    virtual void ProcessOnSubmitJob()
    {
        ;
    }

    ///Return map that maps command names to this plugin
    std::map< std::string, cfdVEBaseClass* > GetCommandNameMap();

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_worldDCS;

    std::string m_objectDescription;

    std::string m_network;

protected:
    long m_pos_x;
    long m_pos_y;

    ves::xplorer::cfdObjects* m_dataRepresentation;

    ves::xplorer::Model* m_model;

    bool m_onSceneGraph;

    int m_modID;
    std::string m_objectName;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_dcs;
    ves::xplorer::cfdCursor* m_cursor;
    ves::xplorer::Device* m_device;
    ves::xplorer::cfdSoundHandler* m_soundHandler;
    ves::xplorer::scenegraph::PhysicsSimulator* m_physicsSimulator;
#ifdef VE_SOUND
    osgAL::SoundManager* m_soundManager;
#endif

    ves::open::xml::model::ModelPtr m_xmlModel;
    std::map< std::string, cfdVEBaseClass* > m_ehMap;
};
}
}
}

#define VE_GRAPHICALPLUGIN_CLASS( name ) \
    extern "C" \
    { \
        VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin() \
        { \
            return new name(); \
        } \
    }

#endif // end CFD_VE_BASE_CLASS_H
