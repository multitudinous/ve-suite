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

#ifndef HYPER_LAB_SCENE_H
#define HYPER_LAB_SCENE_H

// --- My Includes --- ///
namespace hyperlab
{
    class Shaders;
}

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class CADEntity;
    class PhysicsSimulator;
}
}
}

// --- OSG Includes --- //
namespace osg
{
    class Node;
    class Group;
    class Geode;
    class MatrixTransform;
    class Texture2D;
    class Texture3D;
    class Camera;
    class TexGenNode;
    class Light;
    class LightSource;
}

namespace hyperlab
{
class HyperLabScene
{
public:
    HyperLabScene(
        ves::xplorer::scenegraph::DCS* pluginDCS,
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ~HyperLabScene();

    //Base Effects
    void DefaultVisuals();

    //Advanced Effects
    void AdvancedVisuals();

    //XRay Effect
    void XRay();

    osg::Light* GetLight();

private:
    void InitializeScene();

    void CreateLights();
    void CreateNodes();
    void CreateShadowTexture();

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;

    hyperlab::Shaders* shader;

    //Variables to set up custom lighting for the scene
    osg::ref_ptr< osg::Light > mLight;
    osg::ref_ptr< osg::LightSource > mLightSource;
    osg::ref_ptr< osg::MatrixTransform > mLightTransform;

    //Variables to set up shadows for the scene
    osg::ref_ptr< osg::Camera > mCamera;
    osg::ref_ptr< osg::Texture2D > mShadow;
    osg::ref_ptr< osg::TexGenNode > mTexgenNode;
    osg::ref_ptr< osg::Group > mShadowedScene;

    //The room geometry nodes
    ves::xplorer::scenegraph::CADEntity* mRoom;
    osg::ref_ptr< osg::Node > mAluminumParts;
    osg::ref_ptr< osg::Node > mAluminumPipes;
    osg::ref_ptr< osg::Node > mBlack;
    osg::ref_ptr< osg::Node > mBrown;
    osg::ref_ptr< osg::Node > mCeiling;
    osg::ref_ptr< osg::Geode > mCoronas;
    osg::ref_ptr< osg::Node > mDetails;
    osg::ref_ptr< osg::Node > mFloor;
    osg::ref_ptr< osg::Node > mGlass;
    osg::ref_ptr< osg::Node > mLights;
    osg::ref_ptr< osg::Node > mLtGreen;
    osg::ref_ptr< osg::Node > mLtGrey;
    osg::ref_ptr< osg::Node > mOrange;
    osg::ref_ptr< osg::Node > mRed;
    osg::ref_ptr< osg::Node > mRedBrown;
    osg::ref_ptr< osg::Node > mWalls;
    osg::ref_ptr< osg::Node > mWhiteDucts;
    osg::ref_ptr< osg::Node > mWhitePipes;
    osg::ref_ptr< osg::Node > mYellow;
    
    //The hyper project nodes
    osg::ref_ptr< osg::Node > mBlowerComponents;
    osg::ref_ptr< osg::Node > mBrackets;
    osg::ref_ptr< osg::Node > mCableTray;
    osg::ref_ptr< osg::Node > mCementBase;
    osg::ref_ptr< osg::Node > mCombustorInternals;
    osg::ref_ptr< osg::Node > mCombustorPiping;
    osg::ref_ptr< osg::Node > mCompressorInlet;
    osg::ref_ptr< osg::Node > mFrame;
    osg::ref_ptr< osg::Node > mGroundBolts;
    osg::ref_ptr< osg::Node > mHeatExchanger;
    osg::ref_ptr< osg::Node > mHeatExchangerSweep;
    osg::ref_ptr< osg::Node > mInstrumentation;
    osg::ref_ptr< osg::Node > mLoad;
    osg::ref_ptr< osg::Node > mPlenumPiping;
    osg::ref_ptr< osg::Node > mPlenumSystem;
    osg::ref_ptr< osg::Node > mRailing;
    osg::ref_ptr< osg::Node > mReliefPiping;
    osg::ref_ptr< osg::Node > mReliefPipingAM;
    osg::ref_ptr< osg::Node > mShell;
    osg::ref_ptr< osg::Node > mStack;
    osg::ref_ptr< osg::Node > mTurbineExhaust;
    osg::ref_ptr< osg::Node > mTurbinePostCombustor;
    osg::ref_ptr< osg::Node > mTurbineSupport;

};
} //end hyperlab

#endif //HYPER_LAB_SCENE_H
