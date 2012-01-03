/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

// --- My Includes --- //
#include "AudioTestGP.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <osgDB/FileUtils>
#include <osg/Notify>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>
#include <osg/Geometry>
#include <osg/Geode>
#include <osgUtil/Optimizer>
#include <osgDB/Registry>
#include <osgDB/ReadFile>

#include <osgAudio/FileStream.h>
#include <osgAudio/SoundUpdateCB.h>
#include <osgAudio/SoundRoot.h>
#include <osgAudio/SoundManager.h>
#include <osgAudio/SoundState.h>
#include <osgAudio/Version.h>
#include <osgAudio/SoundNode.h>


using namespace ves::xplorer::scenegraph;
using namespace audio;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
AudioTestGP::AudioTestGP()
    :
    PluginBase()
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "AudioTestUI";
}
////////////////////////////////////////////////////////////////////////////////
AudioTestGP::~AudioTestGP()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AudioTestGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    osg::Node* model = createModel();
    if (!model)
    {
        return;
    }
    
    mDCS->addChild( model );
}
////////////////////////////////////////////////////////////////////////////////
void AudioTestGP::RemoveSelfFromSG()
{
    mOnSceneGraph = false;
    mWorldDCS->removeChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
osg::AnimationPath* AudioTestGP::createAnimationPath(const osg::Vec3& center,float radius,double looptime)
{
    // set up the animation path 
    osg::AnimationPath* animationPath = new osg::AnimationPath;
    animationPath->setLoopMode(osg::AnimationPath::LOOP);
    
    int numSamples = 40;
    float yaw = 0.0f;
    float yaw_delta = 2.0f*osg::PI/((float)numSamples-1.0f);
    float roll = osg::inDegrees(30.0f);
    
    double time=0.0f;
    double time_delta = looptime/(double)numSamples;
    for(int i=0;i<numSamples;++i)
    {
        osg::Vec3 position(center+osg::Vec3(sinf(yaw)*radius,cosf(yaw)*radius,0.0f));
        osg::Quat rotation(osg::Quat(roll,osg::Vec3(0.0,1.0,0.0))*osg::Quat(-(yaw+osg::inDegrees(90.0f)),osg::Vec3(0.0,0.0,1.0)));
        
        animationPath->insert(time,osg::AnimationPath::ControlPoint(position,rotation));
        
        yaw += yaw_delta;
        time += time_delta;
    }
    return animationPath;    
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* AudioTestGP::createBase(const osg::Vec3& center,float radius)
{
    int numTilesX = 10;
    int numTilesY = 10;
    
    float width = 2*radius;
    float height = 2*radius;
    
    osg::Vec3 v000(center - osg::Vec3(width*0.5f,height*0.5f,0.0f));
    osg::Vec3 dx(osg::Vec3(width/((float)numTilesX),0.0,0.0f));
    osg::Vec3 dy(osg::Vec3(0.0f,height/((float)numTilesY),0.0f));
    
    // fill in vertices for grid, note numTilesX+1 * numTilesY+1...
    osg::Vec3Array* coords = new osg::Vec3Array;
    int iy;
    for(iy=0;iy<=numTilesY;++iy)
    {
        for(int ix=0;ix<=numTilesX;++ix)
        {
            coords->push_back(v000+dx*(float)ix+dy*(float)iy);
        }
    }
    
    //Just two colours - black and white.
    osg::Vec4Array* colors = new osg::Vec4Array;
    colors->push_back(osg::Vec4(1.0f,1.0f,1.0f,1.0f)); // white
    colors->push_back(osg::Vec4(0.0f,0.0f,0.0f,1.0f)); // black
    int numColors=colors->size();
    
    
    int numIndicesPerRow=numTilesX+1;
    osg::UByteArray* coordIndices = new osg::UByteArray; // assumes we are using less than 256 points...
    osg::UByteArray* colorIndices = new osg::UByteArray;
    for(iy=0;iy<numTilesY;++iy)
    {
        for(int ix=0;ix<numTilesX;++ix)
        {
            // four vertices per quad.
            coordIndices->push_back(ix    +(iy+1)*numIndicesPerRow);
            coordIndices->push_back(ix    +iy*numIndicesPerRow);
            coordIndices->push_back((ix+1)+iy*numIndicesPerRow);
            coordIndices->push_back((ix+1)+(iy+1)*numIndicesPerRow);
            
            // one color per quad
            colorIndices->push_back((ix+iy)%numColors);
        }
    }
    
    // set up a single normal
    osg::Vec3Array* normals = new osg::Vec3Array;
    normals->push_back(osg::Vec3(0.0f,0.0f,1.0f));
    
    osg::Geometry* geom = new osg::Geometry;
    geom->setVertexArray(coords);
    geom->setVertexIndices(coordIndices);
    
    geom->setColorArray(colors);
    geom->setColorIndices(colorIndices);
    geom->setColorBinding(osg::Geometry::BIND_PER_PRIMITIVE);
    
    geom->setNormalArray(normals);
    geom->setNormalBinding(osg::Geometry::BIND_OVERALL);
    
    geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,coordIndices->size()));
    
    osg::Geode* geode = new osg::Geode;
    geode->addDrawable(geom);
    
    return geode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* AudioTestGP::createMovingModel(const osg::Vec3& center, float radius)
{
    float animationLength = 10.0f;
    
    osg::AnimationPath* animationPath = createAnimationPath(center,radius,animationLength);
    
    osg::Group* model = new osg::Group;
    
    osg::Node* glider = osgDB::readNodeFile("glider.osg");
    if (glider)
    {
        const osg::BoundingSphere& bs = glider->getBound();
        
        osg::BoundingSphere::value_type size = radius/bs.radius()*0.3f;
        osg::MatrixTransform* positioned = new osg::MatrixTransform;
        positioned->setDataVariance(osg::Object::STATIC);
        positioned->setMatrix(osg::Matrix::translate(-bs.center())*
                              osg::Matrix::scale(size,size,size)*
                              osg::Matrix::rotate(osg::inDegrees(-90.0f),0.0f,0.0f,1.0f));
        
        positioned->addChild(glider);
        
        // Create a sound update callback and attach a sound state to it
        osg::ref_ptr< osg::Group > group = new osg::Group;
        osg::ref_ptr< osgAudio::SoundUpdateCB > soundCB = new osgAudio::SoundUpdateCB();
        soundCB->setSoundState( createSoundState("bee.wav") );
        glider->setUpdateCallback( soundCB.get() );
        group->addChild(positioned);
        
        osg::PositionAttitudeTransform* xform = new osg::PositionAttitudeTransform;		 
        xform->setUpdateCallback(new osg::AnimationPathCallback(animationPath,0.0,1.0));
        xform->addChild(group.get());
        
        model->addChild(xform);
    }
    
    osg::Node* cessna = osgDB::readNodeFile("cessna.osg");
    if (cessna)
    {
        const osg::BoundingSphere& bs = cessna->getBound();
        
        osg::BoundingSphere::value_type size = radius/bs.radius()*0.3f;
        osg::MatrixTransform* positioned = new osg::MatrixTransform;
        positioned->setDataVariance(osg::Object::STATIC);
        positioned->setMatrix(osg::Matrix::translate(-bs.center())*
                              osg::Matrix::scale(size,size,size)*
                              osg::Matrix::rotate(osg::inDegrees(180.0f),0.0f,0.0f,1.0f));
        
        positioned->addChild(cessna);
        
        osg::MatrixTransform* xform = new osg::MatrixTransform;
        xform->setUpdateCallback(new osg::AnimationPathCallback(animationPath,0.0f,2.0));
        xform->addChild(positioned);
        
        model->addChild(xform);
    }
    
    return model;
}
////////////////////////////////////////////////////////////////////////////////
osgAudio::SoundState* AudioTestGP::createSoundState(const std::string& file)
{
    // Create a sample, load a .wav file.
    osgAudio::Sample* sample = 
    mSoundManager->getSample(file.c_str(), false);
    // Create a named sound state.
    osgAudio::SoundState* sound_state = new osgAudio::SoundState( file );
    // Allocate a hardware soundsource to this soundstate (priority 10)
    sound_state->allocateSource(10, false);
    // Let the soundstate use the sample we just created
    sound_state->setSample(sample);
    
    // Set its gain (volume) to 0.9
    sound_state->setGain(0.9f);
    
    sound_state->setReferenceDistance(70);
    
    // Set its pitch to 1 (normal speed)
    sound_state->setPitch(1);
    
    // Make it play
    sound_state->setPlay(true);
    
    // The sound should loop over and over again
    sound_state->setLooping(true);
    
    // Add the soundstate to the sound manager, so we can find it later on if we want to
    mSoundManager->addSoundState(sound_state);
    
    return sound_state;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* AudioTestGP::createModel()
{
    osg::Vec3 center(0.0f,0.0f,0.0f);
    float radius = 100.0f;
    
    osg::Group* root = new osg::Group;
    
    root->addChild(createMovingModel(center,radius*0.8f));
    
    root->addChild(createBase(center-osg::Vec3(0.0f,0.0f,radius*0.5),radius));
    
    return root;
}
////////////////////////////////////////////////////////////////////////////////
