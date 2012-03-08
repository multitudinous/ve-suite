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

// --- VES Includes --- //
#include <ves/conductor/qt/UIManager.h>
#include <ves/conductor/qt/UIElement.h>
#include <ves/conductor/qt/UIUpdateCallback.h>

#include <ves/xplorer/eventmanager/InteractionEvent.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>

#include <gmtl/Matrix.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
//#include <osg/Geometry>
#include <osg/Group>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/StateAttribute>
#include <osg/Geometry>
#include <osg/Projection>
#include <osg/NodeCallback>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/AnimationPath>
#include <osg/io_utils>
#include <osg/Camera>

#include <osgUtil/CullVisitor>

#include <osgDB/WriteFile>

// --- STL Includes --- //
#include <iostream>

// --- Boost Includes --- //
#include <boost/concept_check.hpp>

//#define VES_QT_RENDER_DEBUG
//#include <osg/Texture2D>

using namespace ves::xplorer::scenegraph;
namespace vxs = ves::xplorer::scenegraph;

namespace ves
{
namespace conductor
{

vprSingletonImp( UIManager );

////////////////////////////////////////////////////////////////////////////////
UIManager::UIManager() :
    mUIUpdateCallback( new UIUpdateCallback() ),
    mInitialized( false ),
    mLeft( 0 ),
    mRight( 640 ),
    mBottom( 0 ),
    mTop( 480 ),
    mRectangleDirty( false ),
    mToggleVisibility( false ),
    mHide( false ),
    mShow( false ),
    mMinimize( false ),
    mUnminimize( false ),
    mOpacity( 0.8f ),
    mDxPointer( 0 ),
    mDyPointer( 0 ),
    mDzPointer( 0 ),
    mCurrentXPointer( 0 ),
    mCurrentYPointer( 0 ),
    mCurrentZPointer( 0 ),
    mMinimizeXOffset( 10.0f ),
    mMoveElement( 0 ),
    mMouseInsideUI( true ), // We start out true, since no Qt events will happen
                            // if we start out false. And that means no UI would
                            // ever appear.
    m_lineSegmentIntersector( new osgUtil::LineSegmentIntersector( 
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),
    m_selectedUIElement( 0 ),
    m_updateBBoxes( false ),
    m_bringToFront( 0 ),
    m_isDesktopMode( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() ),
    m_isWandIntersection( false ),
    m_useRegionDamaging( false ),
    m_logger( Poco::Logger::get("conductor.EventDebug") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    // Register signals
    ves::xplorer::eventmanager::EventManager* evm = ves::xplorer::eventmanager::EventManager::instance();
    using ves::xplorer::eventmanager::SignalWrapper;

    evm->RegisterSignal(
            new SignalWrapper< voidBoolSignalType >( &mUIEnterLeaveSignal ),
            "UIManager.EnterLeaveUI" );

    CONNECTSIGNALS_2( "%.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), &UIManager::SetStartEndPoint,
                     mConnections, any_SignalType, normal_Priority );
    
    //CONNECTSIGNALS_2( "Wand.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), &UIManager::SetStartEndPoint,
    //                 m_connections, any_SignalType, normal_Priority );
                     
    // Connect slots to external signals
    CONNECTSIGNALS_0( "%HideShowUI%", void (), &UIManager::ToggleVisibility, mConnections,
                      any_SignalType, highest_Priority);

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.MouseMove", bool ( int, int, int, int ),
                     ves::xplorer::eventmanager::BooleanPropagationCombiner,
                     &UIManager::MouseMoveEvent, mInputConnections,
                     any_SignalType,highest_Priority );

    CONNECTSIGNALS_4_COMBINER( "%Mouse.ButtonPress%",bool ( gadget::Keys, int, int, int ),
                      ves::xplorer::eventmanager::BooleanPropagationCombiner,
                      &UIManager::ButtonPressEvent, mInputConnections,
                      button_SignalType, highest_Priority );

    CONNECTSIGNALS_4_COMBINER( "%Mouse.ButtonRelease%",bool ( gadget::Keys, int, int, int ),
                      ves::xplorer::eventmanager::BooleanPropagationCombiner,
                      &UIManager::ButtonReleaseEvent, mInputConnections,
                      button_SignalType, highest_Priority );

    CONNECTSIGNALS_5_COMBINER( "%Mouse.DoubleClick%", bool( gadget::Keys, int, int, int, int ),
                      ves::xplorer::eventmanager::BooleanPropagationCombiner,
                      &UIManager::MouseDoubleClickEvent, mInputConnections,
                      button_SignalType, highest_Priority );

    CONNECTSIGNALS_3_COMBINER( "%KeyPress%", bool ( gadget::Keys, int, char ),
                      ves::xplorer::eventmanager::BooleanPropagationCombiner,
                      &UIManager::KeyPressEvent, mInputConnections,
                      keyboard_SignalType, highest_Priority );

    CONNECTSIGNALS_3_COMBINER( "%KeyRelease%", bool ( gadget::Keys, int, char ),
                      ves::xplorer::eventmanager::BooleanPropagationCombiner,
                      &UIManager::KeyReleaseEvent, mInputConnections,
                      keyboard_SignalType, highest_Priority );

    CONNECTSIGNALS_5_COMBINER( "%Mouse.Scroll%", bool ( int, int, int, int, int ),
                      ves::xplorer::eventmanager::BooleanPropagationCombiner,
                      &UIManager::MouseScrollEvent, mInputConnections,
                      input_SignalType, highest_Priority );
           
    ///Setup the wand now
    CONNECTSIGNALS_4( "%Wand.ButtonPress%", void( gadget::Keys, int, int, int ),
                              &UIManager::ButtonPressEvent, mInputConnections,
                              button_SignalType, highest_Priority );
    
    CONNECTSIGNALS_4( "%Wand.ButtonRelease%", void( gadget::Keys, int, int, int ),
                              &UIManager::ButtonReleaseEvent, mInputConnections,
                              button_SignalType, highest_Priority );
    
    CONNECTSIGNALS_5( "%Wand.DoubleClick%", void( gadget::Keys, int, int, int, int ),
                              &UIManager::MouseDoubleClickEvent, mInputConnections,
                              button_SignalType, highest_Priority );

    CONNECTSIGNALS_4( "Wand.WandMove", void( int, int, int, int ),
                              &UIManager::MouseMoveEvent, mInputConnections,
                              any_SignalType,highest_Priority );
    
    // Force input signal monopoly to agree with default state of mMouseInsideUI
    _monopolizeInput( mMouseInsideUI );
}
////////////////////////////////////////////////////////////////////////////////
UIManager::~UIManager()
{
    if( mUIGroup.valid() )
    {
        mUIGroup->removeUpdateCallback( mUIUpdateCallback.get() );
    }

    //std::cout << this->referenceCount() << std::endl;
    // Delete all UIElements of which we've taken charge
    // Note that these were not allocated inside this class, but the class
    // interface specifies that it takes ownership of these objects
    ElementMap_type::const_iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        //delete ( *map_iterator ).second;
    }
    mElements.clear();
    // All other memory allocated on the heap by this class should be attached
    // to an osg::ref_ptr and so should automatically manage its lifetime
    //std::cout << " UI manager destructor" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* UIManager::AddElement( UIElement* element )
{
    //Store the switch node so that it can be added to mUIGroup during the
    //next update traversal.
    osg::Geode* geode = element->GetGeode();
    if( m_isDesktopMode )
    {
        mNodesToAdd.push_back( geode );
    }
    else
    {
        m_rttQuadTransform = new osg::PositionAttitudeTransform();
        m_rttQuadTransform->addChild( geode );
        //m_rttQuadTransform->setUpdateCallback( new ves::xplorer::scenegraph::HeadPositionCallback( gmtl::Point3d( 0.0, 8.0, -2.0 ) ) );
        mNodesToAdd.push_back( m_rttQuadTransform.get() );
    }

    // Move the element so that its top-left corer is at the top-left portion of the
    // UI area
    double elementHeight = element->GetElementHeight();
    element->MoveCanvas( 0, mTop - elementHeight, 0 );

    //mElementPositionsOrtho2D[ element ] = _computeMouseBoundsForElement( element );
    m_updateBBoxes = true;

    mElements[ geode ] = element;

    osg::ref_ptr<TextureSubloader> subloader = new TextureSubloader;
    m_subloaders[ element ] = subloader;
    m_zOrder.push_front( element );

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::RemoveElement( osg::ref_ptr<osg::Geode> geode )
{
    boost::ignore_unused_variable_warning( geode );
    // Search through to find geode, then delete UIElement, geode, and
    // the swicth and transforms in its sub-branch, then erase
    // entry from map
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::RemoveAllElements()
{
    ElementMap_type::iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        delete map_iterator->second;
    }

    mElements.clear();
    m_subloaders.clear();
    m_zOrder.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::Update()
{
    // Ensure that initialization has already happened
    if( !mInitialized )
    {
        //Initialize();
        return;
    }

    // Insert any new elements added since last update into the scenegraph
    if( mNodesToAdd.size() > 0 && mElements.size() > 0 )
    {
        _insertNodesToAdd();
    }

    if( m_bringToFront )
    {
        mUIGroup->removeChild( m_bringToFront );
        mUIGroup->addChild( m_bringToFront );
        m_bringToFront = 0;
    }

    // Update all of the bounding boxes for the uis
    if( m_updateBBoxes )
    {
        //UpdateElementBoundingBoxes();
        m_updateBBoxes = false;
    }
    
    // Update the UI's rectangle if it has been dirtied.
    if( mRectangleDirty )
    {
        //mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
        //                                              mBottom, mTop ) );
        //mProjection->setMatrix( mTempProj );
        
        mRectangleDirty = false;
    }

    // Do hide/show operations
    if( mToggleVisibility )
    {
        if( mUIGroup->getValue( 0 ) )
        {
            mShow = false;
            mHide = true;
        }
        else
        {
            mShow = true;
            mHide = false;
        }

        mToggleVisibility = false;
    }

    // Check visibility of UI branch before bothering with repaints
    if( mUIGroup->getValue( 0 ) )
    {
        m_opacityUniform->set( mOpacity );
        _repaintChildren();
    }

    if( mShow )
    {
        _showAll();
    }
    else if( mHide )
    {
        //osgDB::ReaderWriter::Options* options = new osgDB::ReaderWriter::Options;
        //options->setOptionString( "includeImageFileInIVEFile" );
        //osgDB::writeNodeFile( *m_sceneDebugCamera, "outfile.ive", options );
        //osgDB::writeNodeFile( *mUIGroup, "outfile.ive", options );
        //mHide = false;
        _hideAll();
    }

    if( mMinimize )
    {
        _doMinimize();
    }

    if( mUnminimize )
    {
        _doUnminimize();
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::HideAllElements()
{
    // May not be able to touch scenegraph directly at the moment;
    // Set hide flag to be discovered during update
    mHide = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ShowAllElements( bool showOnlyActive )
{
    boost::ignore_unused_variable_warning( showOnlyActive );
    // May not be able to touch scenegraph directly at the moment;
    // Set show flag to be discovered during update
    mShow = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ToggleVisibility()
{
    // May not be able to touch scenegraph directly at the moment;
    // Set visibility flag to be discovered during update
    mToggleVisibility = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetRectangle( int left, int right, int bottom, int top )
{
    mRectangleDirty = true;
    mLeft = left;
    mRight = right;
    mBottom = bottom;
    mTop = top;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::Initialize( osg::Group* )
{
    // Only allow initialization to happen once.
    if( mInitialized )
    {
        return;
    }

    mUIGroup = new osg::Switch();
    mUIGroup->setName( "Qt UI Group" );
    mUIGroup->setUpdateCallback( mUIUpdateCallback.get() );

    //Setup the shaders
    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->setName( "VS UI Quad Program" );

    if( m_isDesktopMode )
    {
        osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
        std::string vertexSource =
            "void main() \n"
            "{ \n"
            //Ignore MVP transformation as vertices are already in Normalized Device Coord.
            "gl_Position = gl_Vertex; \n"
            "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
            "gl_FrontColor = gl_Color;\n"
            "} \n";
        
        vertexShader->setType( osg::Shader::VERTEX );
        vertexShader->setShaderSource( vertexSource );
        vertexShader->setName( "VS UI Quad Vertex Shader" );
        program->addShader( vertexShader.get() );
    }

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
    "uniform sampler2D baseMap;\n"
    "uniform vec2 mousePoint;\n"
    "uniform vec3 glowColor;\n"
    "uniform float opacityVal;\n"
    "uniform float aspectRatio;\n"

    "void main()\n"
    "{\n"
        "vec3 baseColor = texture2D( baseMap, gl_TexCoord[ 0 ].st ).rgb;\n"

        "vec2 v = abs( gl_TexCoord[ 0 ].st - mousePoint );\n"
        "v.t *= aspectRatio;\n"
        "vec2 v2 = v * v;\n"

        "float radius = 0.025;\n"
        "float r2 = radius * radius;\n"

        //Equation of a circle: (x - h)^2 + (y - k)^2 = r^2
        "if( ( v2.s + v2.t ) <= r2 )\n"
        "{\n"
            "float grad = smoothstep( 0.0, r2, v2.s + v2.t );\n"
            "baseColor = mix( vec3( 1.0, 0.0, 0.0 ), baseColor, grad );\n"
        "}\n"

        "gl_FragData[ 0 ] = vec4( baseColor, opacityVal );\n"
        "gl_FragData[ 1 ] = vec4( glowColor, opacityVal );\n"
    "}\n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS UI Quad Fragment Shader" );
    program->addShader( fragmentShader.get() );

    ///This is a major hack to get around the issue that OSG likes to cull
    ///objects that are on the near plane when a camera is not involved
    if( m_isDesktopMode )
    {
        osg::Camera* postRenderCamera = new osg::Camera();
        postRenderCamera->setName( "Post Render UI Desktop Camera" );
        postRenderCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
        postRenderCamera->setRenderOrder( osg::Camera::POST_RENDER, 1 );
        postRenderCamera->setClearMask( GL_DEPTH_BUFFER_BIT );
        postRenderCamera->setComputeNearFarMode(
            osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
        postRenderCamera->setCullingActive( false );
        postRenderCamera->setThreadSafeRefUnref( true );
        postRenderCamera->setViewMatrix( osg::Matrix::identity() );
        postRenderCamera->setProjectionMatrix( osg::Matrix::identity() );
        ///This postRenderCamera is added to the scene graph in the
        ///void UIManager::AddUIToNode( osg::Group* node ) function. Again
        ///this is only needed in desktop mode to correct the ui rendering 
        ///problems.
        postRenderCamera->addChild( mUIGroup.get() );
    }


    osg::ref_ptr< osg::StateSet > stateset = mUIGroup->getOrCreateStateSet();
    {
        stateset->setRenderBinDetails( 99, "RenderBin" );

        //stateset->setNestRenderBins( false );
        //Set depth test to always pass and don't write to the depth buffer
        stateset->setMode(
                          GL_LIGHTING,
                          osg::StateAttribute::OFF |
                          osg::StateAttribute::OVERRIDE );
        stateset->setMode(
                          GL_DEPTH_TEST,
                          osg::StateAttribute::OFF |
                          osg::StateAttribute::OVERRIDE );
        /*osg::ref_ptr< osg::Depth > depth = new osg::Depth();
        depth->setFunction( osg::Depth::ALWAYS );
        depth->setWriteMask( true );
        stateset->setAttributeAndModes( depth.get(), 
            osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );*/
            
        mUIGroup->setCullingActive( false );
    }

    //Create stateset for adding texture
    osg::StateAttribute::GLModeValue glModeValue =
        osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED |
        osg::StateAttribute::OVERRIDE;
    stateset->setAttributeAndModes( program.get(), glModeValue );
    stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );

    {
        m_opacityUniform = new osg::Uniform( "opacityVal", mOpacity );
        stateset->addUniform( m_opacityUniform.get() );
    }

    {
        m_mousePointUniform = 
            new osg::Uniform( "mousePoint", osg::Vec2d( -1.0, -1.0 ) );
        stateset->addUniform( m_mousePointUniform.get() );
    }

    {
        float uiAspectRatio = 1.0;
        m_aspectRatioUniform = 
            new osg::Uniform( "aspectRatio", uiAspectRatio );
        stateset->addUniform( m_aspectRatioUniform.get() );
    }

    {
        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
        bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                        osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
        stateset->setMode( GL_BLEND, glModeValue );
        stateset->setAttributeAndModes( bf.get(), glModeValue );
    }

    mInitialized = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_insertNodesToAdd()
{
    std::vector< osg::ref_ptr< osg::Node > >::iterator vec_iterator;
    for( vec_iterator = mNodesToAdd.begin();
            vec_iterator != mNodesToAdd.end();
            ++vec_iterator )
    {
        osg::Node* node = ( *vec_iterator ).get();
        mUIGroup->addChild( node );

        osg::Geode* tempGeode = 0;
        if( m_isDesktopMode )
        {
            tempGeode = node->asGeode();
        }
        else
        {
            tempGeode = node->asGroup()->getChild( 0 )->asGeode();
        }

        ElementMap_type::const_iterator iter = mElements.find( tempGeode );

        if( iter != mElements.end() )
        {
            osg::ref_ptr<osg::Texture2D> texture = new osg::Texture2D;
            UIElement* element = iter->second;
            osg::Image* img = new osg::Image;
            img->allocateImage(element->GetImageWidth(), element->GetImageHeight(), 1, GL_RGB, GL_UNSIGNED_BYTE);
            texture->setImage( img );
            texture->setSubloadCallback( m_subloaders[ element ].get() );
            tempGeode->getOrCreateStateSet()->setTextureAttributeAndModes(0, texture.get(), osg::StateAttribute::ON);
        }
        else
        {
            std::cout << "UIManager::_insertNodesToAdd : no elements yet " << mElements.size() << std::endl;
        }

    }

    mNodesToAdd.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_repaintChildren()
{
    ///Update all of the elements and textures
    for( std::list< UIElement* >::const_iterator z_order = m_zOrder.begin(); 
        z_order != m_zOrder.end(); ++z_order )
    {
        // Check whether this element is currently switched as visible. If not,
        // no need to waste time rendering it.
        UIElement* element = (*z_order);
        if( element->IsVisible() )
        {
            float uiAspectRatio =
                float( element->GetImageHeight() ) / float( element->GetImageWidth() );
            m_aspectRatioUniform->set( uiAspectRatio );
            element->Update();

            ///This code must be left here to correctly update the UI.
            osg::ref_ptr< osg::Image > image_Data = element->RenderElementToImage();

            // Only reset the image if element tells us it has changed since 
            // last time
            if( element->IsDirty() )
            {
                std::map< UIElement*, osg::ref_ptr< TextureSubloader > >::const_iterator 
                    subIter = m_subloaders.find( element );
                osg::ref_ptr< TextureSubloader > subloader = subIter->second.get();
                if( !m_useRegionDamaging )
                {
                    // Use texture subload, but don't use region damaging --
                    // just push the entire texture as a subload
                    subloader->AddUpdate( image_Data.get(), 0, 0 );
                }
                else
                {
                    std::vector< std::pair< osg::ref_ptr<osg::Image>, std::pair<int, int> > >
                            regions = element->GetDamagedAreas();
                    for( size_t index = 0; index < regions.size(); ++index )
                    {
                        std::pair< osg::ref_ptr<osg::Image>, std::pair<int, int> > region =
                                regions.at( index );
                        subloader->AddUpdate( region.first.get(),
                                                region.second.first,
                                                region.second.second );
                    }
                    
                    if( regions.empty() )
                    {
                        subloader->ClearData();
                    }
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_sendEvent()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_hideAll()
{
    mUIGroup->setAllChildrenOff();
    mHide = false;

    // Make mouse/keyboard monopolizing state consistent with no UI
    mMouseInsideUI = false;
    mUIEnterLeaveSignal( false );
    _monopolizeInput( false );
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_showAll()
{
    //if( !showOnlyActive )
    {
        ElementMap_type::const_iterator map_iterator;
        for( map_iterator = mElements.begin(); map_iterator != mElements.end();
                ++map_iterator )
        {
            ShowElement( map_iterator->second );
        }
    }

    mUIGroup->setAllChildrenOn();
    mShow = false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::UpdateElementBoundingBoxes()
{
    ElementMap_type::const_iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
        ++map_iterator )
    {
        mElementPositionsOrtho2D[ map_iterator->second ] = _computeMouseBoundsForElement( map_iterator->second );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetProjectionMatrix( osg::Matrixd& matrix )
{
    if( matrix != mTempProj )
    {
        mRectangleDirty = true;
        mTempProj = matrix;
    }
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::Ortho2DTestPointerCoordinates( int x, int y )
{
    m_selectedUIElement = 0;
    ///Handle the test for non desktop mode
    if( !m_isDesktopMode )
    {
        if( TestWandIntersection() )
        {
            ElementMap_type::const_iterator iter = 
            mElements.find( m_selectedUINode->asGeode() );
            if( iter != mElements.end() )
            {
                m_selectedUIElement = iter->second;
            }
            return true;
        }
        return false;
        //return m_isWandIntersection;
    }
    
    // Walk through every visible quad we own, in descending z-order,
    // and see if the point lies on it
    UIElement* tempElement = 0;
    //ElementMap_type::const_iterator map_iterator = mElements.begin();
    std::list<UIElement*>::const_iterator list_iterator = m_zOrder.begin();
    //while( map_iterator != mElements.end() )
    while( list_iterator != m_zOrder.end() )
    {
        // If the quad isn't visible, treat it as though the pointer can't be
        // over it
        tempElement = *list_iterator;//map_iterator->second;
        if( tempElement->IsVisible() )
        {
            if( tempElement->TestQuadIntersection( x, y ) )
            {
                m_selectedUIElement = tempElement;
                return true;
            }
        }
        //++map_iterator;
        ++list_iterator;
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::Test3DPointerCoordinates( int& x, int& y )
{
    m_selectedUIElement = 0;
    ///Handle the test for non desktop mode
    if( !m_isDesktopMode )
    {
        if( TestWandIntersection() )
        {
            ElementMap_type::const_iterator iter = 
            mElements.find( m_selectedUINode->asGeode() );
            if( iter != mElements.end() )
            {
                m_selectedUIElement = iter->second;
                m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
            }
            m_isWandIntersection = true;
            return true;
        }
        m_isWandIntersection = false;
        return false;
    }
    m_isWandIntersection = false;
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetOpacity( float opacity )
{
    if( ( opacity >= 0.0f ) && ( opacity <= 1.0f ) )
    {
        mOpacity = opacity;
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::MinimizeAllElements()
{
    mMinimize = true;

    // Ensure that everything gets minimized
    //mMinimizeElement = false;
    m_MinimizeElements.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_doMinimize()
{
    if( !m_MinimizeElements.empty() )
    {
        // Minimize only what's been placed in m_MinimizeElements
        std::vector< UIElement* >::iterator it = m_MinimizeElements.begin();
        while( it != m_MinimizeElements.end() )
        {
            _doMinMaxElement( *it, true );
            ++it;
            //mMinimizeElement = 0;
        }
        m_MinimizeElements.clear();
    }
    else
    {
        // Minimize everything
        ElementMap_type::const_iterator map_iterator;
        for( map_iterator = mElements.begin(); map_iterator != mElements.end();
                ++map_iterator )
        {
            // reset the minimizeOffset
            mMinimizeXOffset = 10.0f;

            // Don't try to minimize something that's already minimized
            if( !map_iterator->second->IsMinimized() )
            {
                _doMinMaxElement( map_iterator->second, true );
            }
        }
    }

    // Make mouse/keyboard monopolizing state consistent with this element's
    // disappearance
    if( !Ortho2DTestPointerCoordinates( mCurrentXPointer, mCurrentYPointer ) )
    {
        mMouseInsideUI = false;
        mUIEnterLeaveSignal( false );
        _monopolizeInput( false );
    }

    mMinimize = false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_doMinMaxElement( UIElement* element, bool minimize )
{
    // Padding between elements and padding between elements and edge of window
    float xPadding = 10.0f;
    float yPadding = 10.0f;

    // Scale factor for minimization
    float downScale = 0.15f;

    osg::Vec4 const& uiCorners = element->GetUICorners();
    osg::AnimationPath::ControlPoint c1;

    if( minimize )
    {
        // Let element know it is being minimized. We query this state occasionally.
        // Some elements may choose to render their contents differently when
        // minimized; eg. they may stop updating or they may display a condensed
        // "view only" widget set.
        element->SetMinimized( true );

        c1.setPosition( osg::Vec3f( mMinimizeXOffset, yPadding, 0.0f ) );
        float xSize = downScale * (uiCorners[ 1 ] - uiCorners[ 0 ]);
        float ySize = downScale * (uiCorners[ 3 ] - uiCorners[ 2 ]);
        c1.setScale( osg::Vec3f( xSize, ySize, 1.0 ) );
        osg::Matrixf tempMatrix;
        c1.getMatrix( tempMatrix );
        element->PushUIMatrix( tempMatrix );

        mMinimizeXOffset += downScale * ( element->GetElementWidth() ) + xPadding;
        m_minimizedElements.push_back( element );
    }
    else // maximize
    {
        // Temporary hack to deal with mMinimizeOffset. There really needs to be
        // a list of all minimized elements (sorted left to right) and accompanying logic
        // to place a new minimized element and to shift elements left when a minimized
        // element in the pack gets maximized.
        mMinimizeXOffset -= downScale * ( element->GetElementWidth() ) + xPadding;

        // Tell element it is no longer minimized so it can adjust its render
        // policy and widget placement accordingly.
        element->SetMinimized( false );

        osg::Matrixf tempUIMat = element->PopUIMatrix();
        c1.setPosition( tempUIMat.getTrans() );
        c1.setScale( tempUIMat.getScale() );

        // Run through list of minimized elements until we find the one we
        // just un-minimized. Translate the position of all elements after that
        // one to the left by the width of the un-minimized element.
        bool after = false;
        float distance = -1 * ( downScale * element->GetElementWidth() + xPadding );
        osg::Matrixf moveLeft;
        moveLeft.setTrans( distance, 0.0f, 0.0f );
        osg::Matrixf current;
        std::list< UIElement* >::iterator minElemIterator;
        for( minElemIterator = m_minimizedElements.begin();
             minElemIterator != m_minimizedElements.end();
             ++minElemIterator )
        {
            if( after )
            {
                current = (*minElemIterator)->GetUIMatrix();
                (*minElemIterator)->PopUIMatrix();
                (*minElemIterator)->PushUIMatrix( current * moveLeft );
            }

            if( *minElemIterator == element )
            {
                after = true;
            }
        }

        m_minimizedElements.remove( element );
    }

    m_updateBBoxes = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_doUnminimize()
{
    if( !m_UnminimizeElements.empty() )
    {
        // Minimize only what's in m_UnminimizeElements
        std::vector< UIElement* >::iterator it = m_UnminimizeElements.begin();
        while( it != m_UnminimizeElements.end() )
        {
            _doMinMaxElement( *it, false );
            ++it;
        }
        m_UnminimizeElements.clear();
    }
    else
    {
        // Unminimize everything
        ElementMap_type::const_iterator map_iterator;
        for( map_iterator = mElements.begin(); map_iterator != mElements.end();
                ++map_iterator )
        {
            // Only do operation if element is minimized
            if( map_iterator->second->IsMinimized() )
            {
                _doMinMaxElement( map_iterator->second, false );
            }
        }
    }

    mUnminimize = false;
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec4 UIManager::_computeMouseBoundsForElement( UIElement* element )
{
    //This function basically is creating the screen coordinates to do
    //mouse testing against to see if the mouse is over the ui
    osg::ref_ptr< osg::Geode > geode = element->GetGeode();
    osg::Vec3Array* vertexArray = 
        static_cast< osg::Vec3Array* >( geode->getDrawable( 0 )->asGeometry()->getVertexArray() );
    osg::Vec3& ll = vertexArray->at( 0 );
    osg::Vec3& ur = vertexArray->at( 2 );

    osg::Matrixd const& windowMat = 
        ves::xplorer::scenegraph::SceneManager::instance()->
        GetCurrentGLTransformInfo()->GetWindowMatrixOSG();
    osg::Vec3 min = ll * windowMat;
    osg::Vec3 max = ur * windowMat;

    // Return in the form (left, right, bottom, top)
    return osg::Vec4( min.x(), max.x(), min.y(), max.y() );
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::InitiateMoveElement( UIElement* element )
{
    mMoveElement = element;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::MinimizeElement( UIElement* element )
{
    mMinimize = true;
    m_MinimizeElements.push_back( element );
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::UnminimizeElement( UIElement* element )
{
    mUnminimize = true;
    m_UnminimizeElements.push_back( element );
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::HideElement( UIElement* element )
{
    // Simple hide. May want to add animation later
    element->SetVisible( false );

    // Make mouse/keyboard monopolizing state consistent with this element's
    // disappearance
    if( !Ortho2DTestPointerCoordinates( mCurrentXPointer, mCurrentYPointer ) )
    {
        mMouseInsideUI = false;
        mUIEnterLeaveSignal( false );
        _monopolizeInput( false );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ShowElement( UIElement* element )
{
    // Simple show. May want to add animation later
    element->SetVisible( true );
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ToggleElementVisibility( UIElement* element )
{
    if( element->IsVisible() )
    {
        HideElement( element );
    }
    else
    {
        ShowElement( element );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::ButtonPressEvent( gadget::Keys button, int x, int y, int state )
{
    //LOG_INFO("UIManager::ButtonPressEvent");
    if( !_okayToSendEvent() )
    {
        return false;
    }

    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return false;
    }
    
    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // CTRL + Middle button toggles display of titlebar
    if( (!minimized)
        && (state & gadget::BUTTON2_MASK) && (state & gadget::CTRL_MASK) )
    {
        m_selectedUIElement->ToggleTitlebar();
        return false;
    }

    // Middle button without CTRL starts a move operation
    if( (!mMoveElement) && (state & gadget::BUTTON2_MASK)
        && (!minimized) && (!(state & gadget::CTRL_MASK))  )
    {
        mMoveElement = m_selectedUIElement;
    }

    // Bring element to top of z-order if it isn't already there
    if( *(m_zOrder.begin()) != m_selectedUIElement )
    {
        // Tell the element that was previously on top that it has been lowered
        (*(m_zOrder.begin()))->Lower();
        // Move the new one to the top and tell it to raise
        m_zOrder.remove( m_selectedUIElement );
        m_zOrder.push_front( m_selectedUIElement );
        m_bringToFront = m_selectedUIElement->GetGeode();
        m_selectedUIElement->Raise();
    }

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        //LOG_INFO( "UIManager::ButtonPressEvent point " << m_intersectionPoint.x() << " " << m_intersectionPoint.z() );
        //LOG_INFO( "UIManager::ButtonPressEvent start point " << m_startPoint.x() << " " << m_startPoint.y() << " " << m_startPoint.z() );
        //LOG_INFO( "UIManager::ButtonPressEvent end point " << m_endPoint.x() << " " << m_endPoint.y() << " " << m_endPoint.z() );
        // Translate mouse coordinates to window coordinates
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        // Flip y mouse coordinate to origin GUI expects
        y = m_selectedUIElement->GetElementHeight() - y;
        m_mousePointUniform->set( m_selectedUIElement->GetTextureCoords( x, y ) );
        m_selectedUIElement->SendButtonPressEvent( button, x, y, state );
        //LOG_INFO("UIManager::SendButtonPressEvent");
    }
    else
    {
        mUnminimize = true;
        //mUnminimizeElement = m_selectedUIElement;
        m_UnminimizeElements.push_back( m_selectedUIElement );
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::ButtonReleaseEvent( gadget::Keys button, int x, int y, int state )
{
    if( !_okayToSendEvent() )
    {
        return false;
    }

    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    // If we're ending an element move, do that and sink the event
    if( mMoveElement )
    {
        m_updateBBoxes = true;
        mMoveElement = 0;
        return false;
    }

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return false;
    }

    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        // Translate mouse coordinates to window coordinates
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        // Flip y mouse coordinate to origin GUI expects
        y = m_selectedUIElement->GetElementHeight() - y;
        m_mousePointUniform->set( m_selectedUIElement->GetTextureCoords( x, y ) );
        m_selectedUIElement->SendButtonReleaseEvent( button, x, y, state );
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::MouseScrollEvent( int deltaX, int deltaY, int x, int y, int state )
{
    if( !_okayToSendEvent() )
    {
        return false;
    }

    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return false;
    }

    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        // Translate mouse coordinates to window coordinates
        osg::Vec3d tempPoint;
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, tempPoint );
        // Flip y mouse coordinate to origin GUI expects
        y = m_selectedUIElement->GetElementHeight() - y;
        m_selectedUIElement->SendScrollEvent( deltaX, deltaY, x, y, state );
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::MouseMoveEvent( int x, int y, int z, int state )
{
    if( !_okayToSendEvent() )
    {
        return false;
    }

    //Test3DPointerCoordinates( x, y );
    
    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    // If an element move operation is in progress, handle that and sink the event
    if( mMoveElement )
    {
        mMoveElement->MoveCanvas( mDxPointer, mDyPointer );
        return false;
    }

    bool OrthoTest = Ortho2DTestPointerCoordinates( x, y );

    // Send out Enter/Leave signal if that state has just changed
    if( OrthoTest && !mMouseInsideUI )
    {
        mMouseInsideUI = true;
        mUIEnterLeaveSignal( true );
        _monopolizeInput( true );
    }
    else if( !OrthoTest && mMouseInsideUI )
    {
        mMouseInsideUI = false;
        mUIEnterLeaveSignal( false );
        _monopolizeInput( false );
    }

    // If we're actually not over a managed quad, do no more
    if( !OrthoTest )
    {
        return false;
    }

    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // Begin a move operation if user is holding down middle button (wheel) and
    // moving at same time. This code duplicates code above that deals with
    // a move operation already in progress. We duplicate the code rather than
    // moving the Ortho2DTestPointerCoordinates test before that code so that
    // we can avoid a somewhat expensive pointercoord test when it isn't necessary.
//    if( (!mMoveElement) && (state & gadget::BUTTON2_MASK)
//        && (!minimized) && (!(state & gadget::CTRL_MASK))  )
//    {
//        mMoveElement = m_selectedUIElement;
//        mMoveElement->MoveCanvas( mDxPointer, mDyPointer );
//        return false;
//    }

    // Only send events if element is visible and not minimzed
    if( visible && ( !minimized ) )
    {
        // Translate mouse coordinates to window coordinates
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        // Flip y mouse coordinate to origin GUI expects
        y = m_selectedUIElement->GetElementHeight() - y;
        m_mousePointUniform->set( m_selectedUIElement->GetTextureCoords( x, y ) );
        m_selectedUIElement->SendMouseMoveEvent( x, y, z, state );
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::MouseDoubleClickEvent( gadget::Keys button, int x, int y, int z, int state )
{
    boost::ignore_unused_variable_warning( z );
    
    if( !_okayToSendEvent() )
    {
        return false;
    }

    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return false;
    }

    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        // Translate mouse coordinates to window coordinates
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        // Flip y mouse coordinate to origin GUI expects
        y = m_selectedUIElement->GetElementHeight() - y;
        m_selectedUIElement->SendDoubleClickEvent( button, x, y, state );
    }
    else
    {
        mUnminimize = true;
    }

    return false;
}

////////////////////////////////////////////////////////////////////////////////
bool UIManager::KeyPressEvent( gadget::Keys key, int modifiers, char unicode )
{
    if( !_okayToSendEvent() )
    {
        return false;
    }

    // Intercept the UI hide/show hotkey.
    if( key == gadget::KEY_F1 )
    {
        // Do nothing here; the work is done in KeyReleaseEvent
        return true;
    }

    // Don't pass on key events if mouse pointer is not inside the UI
    if( !mMouseInsideUI )
    {
        return false;
    }

    ///If we do not have a selected element
    if( !m_selectedUIElement )
    {
        return false;
    }
    
    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        m_selectedUIElement->SendKeyPressEvent( key, modifiers, unicode );
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::KeyReleaseEvent( gadget::Keys key, int modifiers, char unicode )
{
    if( ! _okayToSendEvent() )
    {
        return false;
    }

    // Intercept the UI hide/show hotkey.
    if( key == gadget::KEY_F1 )
    {
        ToggleVisibility();
        return true;
    }

    // Don't pass on key events if mouse pointer is not inside the UI
    if( !mMouseInsideUI )
    {
        return false;
    }

    ///If we do not have a selected element
    if( !m_selectedUIElement )
    {
        return false;
    }
    
    bool visible = m_selectedUIElement->IsVisible();
    bool minimized = m_selectedUIElement->IsMinimized();

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        m_selectedUIElement->SendKeyReleaseEvent( key, modifiers, unicode );
    }


    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::_okayToSendEvent()
{
    // Ignore events if we're not initialized
    if( !mInitialized )
    {
        return false;
    }

    // Check visibility of UI branch before bothering with events
    if( !mUIGroup->getValue( 0 ) )
    {
        return false;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_monopolizeInput( bool monopolize )
{
    if( monopolize )
    {
        // Monopolize all key/button input events
        ves::xplorer::eventmanager::EventManager* evm =
                ves::xplorer::eventmanager::EventManager::instance();

        ves::xplorer::eventmanager::ScopedConnectionList::ConnectionList_type::iterator iter
                = mInputConnections.GetBegin();

        while( iter != mInputConnections.GetEnd() )
        {
            mInputMonopolies.push_back(
                    evm->MonopolizeConnectionStrong( (*iter) ) );
            ++iter;
        }
    }
    else
    {
        // Release all key/button monopolies
        std::vector< boost::shared_ptr<
            ves::xplorer::eventmanager::ConnectionMonopoly >
            >::iterator iter = mInputMonopolies.begin();
        while( iter != mInputMonopolies.end() )
        {
            (*iter).reset();
            ++iter;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Group& UIManager::GetUIRootNode() const
{
    return *(mUIGroup.get());
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::TestWandIntersection()
{
    m_lineSegmentIntersector->reset();
    m_lineSegmentIntersector->setStart( m_startPoint );
    m_lineSegmentIntersector->setEnd( m_endPoint );
    //Get node of that the UI is attached too
    //Do an interesection test only on that node with the wand line
    osgUtil::LineSegmentIntersector::Intersections& intersections =
        ves::xplorer::scenegraph::TestForIntersections( *m_lineSegmentIntersector.get(), GetUIRootNode() );
    
    m_selectedUINode = 0;
    m_selectedUIElement = 0;
    m_intersectionPoint = osg::Vec3d( -10000, -10000, -10000 );

    if( !intersections.empty() )
    {
        //Now do a test to determine where the wand ray is interesting the plane
        //of the UI texture so that we can translate that to an x,y location
        osgUtil::LineSegmentIntersector::Intersection tempIntersection = 
            *(intersections.begin());
        m_intersectionPoint = tempIntersection.getLocalIntersectPoint();
        
        m_selectedUINode = *(tempIntersection.nodePath.rbegin());
        //LOG_INFO( "UIManager::TestWandIntersection " << m_intersectionPoint.x() << " " << m_intersectionPoint.y() );
        //std::cout << "Wand intersection at " << m_intersectionPoint 
        //    << " with the this UI node "
        //    << m_selectedUINode->getName() << std::endl;
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetStartEndPoint( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    m_startPoint = startPoint;
    m_endPoint = endPoint;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetCameraForSceneDebug( osg::Camera* camera )
{
    m_sceneDebugCamera = camera;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::AddUIToNode( osg::Group* node )
{
    if( m_isDesktopMode )
    {
        node->addChild( mUIGroup->getParent( 0 ) );
    }
    else
    {
        node->addChild( mUIGroup.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetRegionDamaging( bool useRegionDamaging )
{
    if( mElements.empty() )
    {
        m_useRegionDamaging = useRegionDamaging;
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::UpdateUIQuadPosition()
{
    if( m_isDesktopMode )
    {
        return;
    }

    if( !_okayToSendEvent() )
    {
        return;
    }
    gmtl::Point3d headPoint = 
        gmtl::makeTrans< gmtl::Point3d >( vxs::SceneManager::instance()->GetHeadMatrix() );
    gmtl::Matrix44d worldMat = 
        vxs::SceneManager::instance()->GetInvertedNavMatrix();//GetGlobalViewMatrix();
    gmtl::Point3d transformPoint = gmtl::Point3d( 0.0, 8.0, -2.0 );
    transformPoint += headPoint;

    transformPoint = worldMat * transformPoint;
    
    gmtl::Quatd invertedQuat = gmtl::makeRot< gmtl::Quatd >( worldMat );
    osg::Quat quat;
    quat.set( invertedQuat.mData[ 0 ], invertedQuat.mData[ 1 ],
             invertedQuat.mData[2],invertedQuat.mData[ 3 ]);
    m_rttQuadTransform->setAttitude( quat );

    m_rttQuadTransform->setPosition(
        osg::Vec3d( transformPoint.mData[ 0 ], transformPoint.mData[ 1 ], 
        transformPoint.mData[ 2 ] ) );
    
}
////////////////////////////////////////////////////////////////////////////////
}
}
