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

// --- OSG Includes --- //
//#include <osg/Geometry>
#include <osg/Group>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/StateAttribute>
#include <osg/Geometry>
//#include <osg/Texture2D>
#include <osg/Projection>
#include <osg/NodeCallback>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/AnimationPath>
#include <osg/io_utils>

// --- STL Includes --- //
#include <iostream>

// --- Boost Includes --- //
#include <boost/concept_check.hpp>

//#define VES_QT_RENDER_DEBUG

using namespace ves::conductor;

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
    mMinimizeElement( 0 ),
    mUnminimizeElement( 0 ),
    mMouseInsideUI( true ), // We start out true, since no Qt events will happen
                            // if we start out false. And that means no UI would
                            // ever appear.
    m_lineSegmentIntersector( new osgUtil::LineSegmentIntersector( 
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),
    m_selectedUIElement( 0 ),
    m_updateBBoxes( false )
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
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        mNodesToAdd.push_back( geode );
    }
    else
    {
        m_rttQuadTransform = new osg::PositionAttitudeTransform();
        m_rttQuadTransform->addChild( geode );
        m_rttQuadTransform->setUpdateCallback( new ves::xplorer::scenegraph::HeadPositionCallback() );
        mNodesToAdd.push_back( m_rttQuadTransform.get() );
    }

    //mElementPositionsOrtho2D[ element ] = _computeMouseBoundsForElement( element );
    m_updateBBoxes = true;

    mElements[ geode ] = element;

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
    if( mNodesToAdd.size() > 0 )
    {
        _insertNodesToAdd();
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

    if( mShow )
    {
        _showAll();
    }
    else if( mHide )
    {
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

    // Check visibility of UI branch before bothering with repaints
    if( mUIGroup->getValue( 0 ) )
    {
        m_opacityUniform->set( mOpacity );
        _repaintChildren();
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
void UIManager::Initialize( osg::Group* parentNode )
{
    //std::cout << "UIManager::Initialize" << std::endl;
    // Only allow initialization to happen once.
    if( mInitialized )
    {
        return;
    }

    //mProjection = new osg::Projection();
    //mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
    //                                              mBottom, mTop ) );

    //osg::MatrixTransform* modelViewMatrix = new osg::MatrixTransform;
    //modelViewMatrix->setMatrix( osg::Matrix::identity() );
    //modelViewMatrix->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

    //mProjection->addChild( modelViewMatrix );

    mUIGroup = new osg::Switch();
    mUIGroup->setDataVariance( osg::Object::DYNAMIC );
    mUIGroup->setUpdateCallback( mUIUpdateCallback.get() );
    //modelViewMatrix->addChild( mUIGroup.get() );

    //parentNode->addChild( mProjection.get() );
    parentNode->addChild( mUIGroup.get() );

    //Setup the shaders
    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->setName( "VS UI Quad Program" );

    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
        std::string vertexSource =
            "void main() \n"
            "{ \n"
            //Ignore MVP transformation as vertices are already in Normalized Device Coord.
            "gl_Position = gl_Vertex; \n"
            "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
            "} \n";
        
        vertexShader->setType( osg::Shader::VERTEX );
        vertexShader->setShaderSource( vertexSource );
        vertexShader->setName( "VS UI Quad Vertex Shader" );
        program->addShader( vertexShader.get() );
    }

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
        "uniform sampler2D baseMap; \n"
        "uniform float opacityVal;\n"
        "uniform vec3 glowColor; \n"
        "uniform vec2 mousePoint; \n"

        "void main() \n"
        "{ \n"
        "vec4 baseColor = texture2D( baseMap, gl_TexCoord[ 0 ].st ); \n"
        // Calculate distance to circle center
        "float d = distance(gl_TexCoord[0].st, mousePoint);\n"
        "vec4 tempColor = baseColor;\n"

        "if( d < 0.05 )\n"
        "{\n"
        //width of "pixel region" in texture coords
        //"   vec2 texCoordsStep = 1.0/(vec2(float(600),float(967))/float(20)); \n"
        //x and y coordinates within "pixel region"
        //"   vec2 pixelRegionCoords = fract(gl_TexCoord[0].st/texCoordsStep);\n"
        ///Radius squared
        "   float radiusSqrd = pow(0.01,2.0);\n"
        ///tolerance
        "   float tolerance = 0.0001;\n"
        "   vec2 powers = pow(abs(gl_TexCoord[0].st - mousePoint),vec2(2.0));\n"
        //Equation of a circle: (x - h)^2 + (y - k)^2 = r^2
        "   float gradient = smoothstep(radiusSqrd-tolerance, radiusSqrd+tolerance, pow(d,2.0) );\n"
        //blend between fragments in the circle and out of the circle defining our "pixel region"
        "   tempColor = mix( vec4(1.0,0.0,0.0,1.0), baseColor, gradient);\n"
        "}\n"
        //"vec2 powers = pow(abs(gl_TexCoord[ 0 ].st - mousePoint),vec2(2.0));\n"
        ///Radius squared
        //"float radiusSqrd = pow(0.003,2.0);\n"
        ///tolerance
        //"float tolerance = 0.0001;\n"
        //Equation of a circle: (x - h)^2 + (y - k)^2 = r^2
        //"float gradient = smoothstep(radiusSqrd-tolerance, radiusSqrd+tolerance, powers.x+powers.y);\n"
        //blend between fragments in the circle and out of the circle defining our "pixel region"
        //"vec4 tempColor = mix( vec4(1.0,0.0,0.0,1.0), baseColor, gradient);\n"
        "tempColor.a = opacityVal;\n"
        "gl_FragData[ 0 ] = tempColor; \n"
        "gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a ); \n"
        "} \n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS UI Quad Fragment Shader" );
    program->addShader( fragmentShader.get() );

    //Set depth test to always pass and don't write to the depth buffer
    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setFunction( osg::Depth::ALWAYS );
    depth->setWriteMask( false );

    //Create stateset for adding texture
	osg::StateAttribute::GLModeValue glModeValue =
		osg::StateAttribute::ON |
		osg::StateAttribute::PROTECTED |
		osg::StateAttribute::OVERRIDE;
    osg::ref_ptr< osg::StateSet > stateset = mUIGroup->getOrCreateStateSet();
    stateset->setRenderBinDetails( 99, "RenderBin" );
    stateset->setAttributeAndModes( depth.get(), glModeValue );
    stateset->setMode( GL_DEPTH_TEST, osg::StateAttribute::OFF );
    stateset->setMode( GL_LIGHTING, glModeValue);
    stateset->setAttributeAndModes( program.get(), glModeValue );
    stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );
    m_opacityUniform = new osg::Uniform( "opacityVal", mOpacity );
    stateset->addUniform( m_opacityUniform.get() );

    m_mousePointUniform = new osg::Uniform( "mousePoint", osg::Vec2d( 0.2, 0.2 ) );
    stateset->addUniform( m_mousePointUniform.get() );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                     osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
    stateset->setMode( GL_BLEND, glModeValue );
    stateset->setAttributeAndModes( bf.get(), glModeValue );

    mInitialized = true;
}
////////////////////////////////////////////////////////////////////////////////
/*bool UIManager::SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent &event )
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

    using ves::xplorer::eventmanager::InteractionEvent;

    // If we're dealing with a mouse event, see if it's over one of our managed
    // quads. If it isn't, we ignore the event.
    if( ( event.EventType == InteractionEvent::pointerMotion ) ||
            ( event.EventType == InteractionEvent::buttonPress ) ||
            ( event.EventType == InteractionEvent::buttonRelease ) )
    {
        // Store off coordinates and deltas
        int tempX = event.X;
        int tempY = event.Y;
        mDxPointer = tempX - mCurrentXPointer;
        mDyPointer = tempY - mCurrentYPointer;
        mCurrentXPointer = tempX;
        mCurrentYPointer = tempY;

        // If an element move operation is in progress, handle that and sink the event
        if( mMoveElement )
        {
            if( event.EventType == InteractionEvent::pointerMotion )
            {
                mMoveElement->MoveCanvas( mDxPointer, mDyPointer );
            }
            else if( event.EventType == InteractionEvent::buttonRelease )
            {
                mElementPositionsOrtho2D[ mMoveElement ] = _computeMouseBoundsForElement( mMoveElement );
                mMoveElement = 0;
            }
            return true;
        }

        if( !Ortho2DTestPointerCoordinates( mCurrentXPointer, mCurrentYPointer ) )
        {
            return false;
        }
    }

    // Currently we have no logic in place to determine *which* element should
    // receive this event. When there are multiple Geodes, the passed event
    // should include a pointer to the Geode that had mouse/wand intersection
    // (for mouse events). We should be storing the focused element somewhere
    // in this manager class for keyboard events.

    ElementMap_type::iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        UIElement* element = map_iterator->second;

        bool visible = element->IsVisible();
        bool minimized = element->IsMinimized();

        // Only send events if element is visible and not minimzed
        if( ( visible ) && ( !minimized ) )
        {
            // Translate mouse coordinates to window coordinates
            // TODO: This may be done better by using the element's entire UIMatrix
            // so that mouse events can be mapped to scaled (but not minimized) windows.
            osg::Vec3 trans = element->GetUIMatrix().getTrans();
            event.X = event.X - trans.x();
            event.Y = event.Y - trans.y();

            // Flip y mouse coordinate to origin GUI expects
            event.Y = static_cast < double > ( mTop ) - event.Y;
            element->SendInteractionEvent( event );
        }
        else if( ( visible ) && ( minimized ) &&
                ( event.EventType == InteractionEvent::buttonPress ) )
        {
            mUnminimize = true;
        }
    }
    return true;
}*/
////////////////////////////////////////////////////////////////////////////////
void UIManager::_insertNodesToAdd()
{
    std::vector< osg::ref_ptr< osg::Node > >::iterator vec_iterator;
    for( vec_iterator = mNodesToAdd.begin();
            vec_iterator != mNodesToAdd.end();
            ++vec_iterator )
    {
        mUIGroup->addChild( ( *vec_iterator ).get() );
        //( *vec_iterator ).release();
    }

    mNodesToAdd.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_repaintChildren()
{
    ElementMap_type::const_iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        // Check whether this element is currently switched as visible. If not,
        // no need to waste time rendering it.
        // FIXME: should only render *active*, visible element?
        UIElement* element = map_iterator->second;
        if( element->IsVisible() )
        {
            element->Update();
            unsigned char* image_Data = element->RenderElementToImage();

            // Only reset the image if element tells us it has changed since 
            // last time
            if( element->IsDirty() )
            {
                osg::StateSet* state = ( *map_iterator ).first
                        ->getOrCreateStateSet();
                osg::Image* image =
                        state->getTextureAttribute( 0, osg::StateAttribute::TEXTURE )
                        ->asTexture()->getImage( 0 );
                image->setImage( element->GetImageWidth(),
                                 element->GetImageHeight(), 1, 4,
                                 GL_BGRA, GL_UNSIGNED_BYTE,
                                 image_Data, osg::Image::NO_DELETE );
                image->dirty();
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
    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
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
    }

    // Walk through every visible quad we own and see if the point lies on it
    UIElement* tempElement;
    ElementMap_type::const_iterator map_iterator = mElements.begin();
    while( map_iterator != mElements.end() )
    {
        // If the quad isn't visible, treat it as though the pointer can't be
        // over it
        tempElement = map_iterator->second;
        if( tempElement->IsVisible() )
        {
            if( tempElement->TestQuadIntersection( x, y ) )
            {
                m_selectedUIElement = tempElement;
                return true;
            }
        }
        ++map_iterator;
    }

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
    mMinimizeElement = false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_doMinimize()
{
    if( mMinimizeElement )
    {
        _doMinMaxElement( mMinimizeElement, true );
        mMinimizeElement = 0;
    }
    else
    {
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

    // Duration in seconds of animation
    //float duration = 0.4f;

    // Animation based on two control points: c0 (current state) and c1 (end state)
    //osg::Matrixf currentMatrix = element->GetUIMatrix();

    osg::Vec4 const& uiCorners = element->GetUICorners();
    osg::AnimationPath::ControlPoint c0( osg::Vec3( uiCorners[ 0 ], uiCorners[ 2 ], 0.0 ) );//currentMatrix.getTrans() );
    c0.setScale( osg::Vec3( uiCorners[ 1 ] - uiCorners[ 0 ], uiCorners[ 3 ] - uiCorners[ 2 ], 0.0 ) );//currentMatrix.getScale() );

    osg::AnimationPath::ControlPoint c1;

    // Set up the second animation control point based on whether this is a
    // min or an unmin operation. Also set the element's UIMatrix to match
    // the end state of the animation.
    if( minimize )
    {
        element->SetMinimized( true );
        c1.setPosition( osg::Vec3f( mMinimizeXOffset, yPadding, 0.0f ) );
        c1.setScale( osg::Vec3f( downScale * (uiCorners[ 1 ] - uiCorners[ 0 ]), downScale * (uiCorners[ 3 ] - uiCorners[ 2 ]), 1.0 ) );
        osg::Matrixf tempMatrix;
        c1.getMatrix( tempMatrix );
        element->PushUIMatrix( tempMatrix );

        mMinimizeXOffset += downScale * ( element->GetElementWidth() ) + xPadding;
    }
    else // maximize
    {
        // Temporary hack to deal with mMinimizeOffset. There really needs to be
        // a list of all minimized elements (sorted left to right) and accompanying logic
        // to place a new minimized element and to shift elements left when a minimized
        // element in the pack gets maximized.
        mMinimizeXOffset -= downScale * ( element->GetElementWidth() ) + xPadding;

        element->SetMinimized( false );
        osg::Matrixf tempUIMat = element->PopUIMatrix();
        c1.setPosition( tempUIMat.getTrans() );
        c1.setScale( tempUIMat.getScale() );
    }

    /*osg::ref_ptr< osg::AnimationPath > path = new osg::AnimationPath;
    path->setLoopMode( osg::AnimationPath::NO_LOOPING );

    path->insert( 0.0f, c0 );
    path->insert( duration, c1 );*/

    //element->SetAnimationPath( path.get() );
    m_updateBBoxes = true;
    //mElementPositionsOrtho2D[ element ] = _computeMouseBoundsForElement( element );
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_doUnminimize()
{
    if( mUnminimizeElement )
    {
        _doMinMaxElement( mUnminimizeElement, false );
        mUnminimizeElement = 0;
    }
    else
    {
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
    mMinimizeElement = element;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::UnminimizeElement( UIElement* element )
{
    mUnminimize = true;
    mUnminimizeElement = element;
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
        // TODO: This may be done better by using the element's entire UIMatrix
        // so that mouse events can be mapped to scaled (but not minimized) windows.
        //osg::Vec4& uiCorners = mElementPositionsOrtho2D[ element ];
        //x = x - uiCorners[ 0 ];
        //y = y - uiCorners[ 2 ];
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        //std::cout << x << " " << trans.x() << " " << y << " " << trans.y() << std::endl;
        // Flip y mouse coordinate to origin GUI expects
        y = static_cast < double > ( mTop ) - y;
        m_mousePointUniform->set( m_selectedUIElement->GetTextureCoords( x, y ) );
        //osg::Vec2d& tempCoords = m_selectedUIElement->GetTextureCoords( x, y );
        //std::cout << y << " " << mTop << std::endl;
        m_selectedUIElement->SendButtonPressEvent( button, x, y, state );
    }
    else
    {
        mUnminimize = true;
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
        // TODO: This may be done better by using the element's entire UIMatrix
        // so that mouse events can be mapped to scaled (but not minimized) windows.
        //osg::Vec4& uiCorners = mElementPositionsOrtho2D[ element ];
        //x = x - uiCorners[ 0 ];
        //y = y - uiCorners[ 2 ];
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        //std::cout << x << " " << trans.x() << " " << y << " " << trans.y() << std::endl;
        // Flip y mouse coordinate to origin GUI expects
        y = static_cast < double > ( mTop ) - y;
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
        // TODO: This may be done better by using the element's entire UIMatrix
        // so that mouse events can be mapped to scaled (but not minimized) windows.
        //osg::Vec4& uiCorners = mElementPositionsOrtho2D[ element ];
        //x = x - uiCorners[ 0 ];
        //y = y - uiCorners[ 2 ];
        osg::Vec3d tempPoint;
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, tempPoint );
        // Flip y mouse coordinate to origin GUI expects
        y = static_cast < double > ( mTop ) - y;
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

    // Only send events if element is visible and not minimzed
    if( ( visible ) && ( !minimized ) )
    {
        // Translate mouse coordinates to window coordinates
        // TODO: This may be done better by using the element's entire UIMatrix
        // so that mouse events can be mapped to scaled (but not minimized) windows.
        //osg::Vec4& uiCorners = mElementPositionsOrtho2D[ element ];
        //x = x - uiCorners[ 0 ];
        //y = y - uiCorners[ 2 ];
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        //std::cout << x << " " << trans.x() << " " << y << " " << trans.y() << std::endl;
        // Flip y mouse coordinate to origin GUI expects
        y = static_cast < double > ( mTop ) - y;
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
        // TODO: This may be done better by using the element's entire UIMatrix
        // so that mouse events can be mapped to scaled (but not minimized) windows.
        //osg::Vec4& uiCorners = mElementPositionsOrtho2D[ element ];
        //x = x - uiCorners[ 0 ];
        //y = y - uiCorners[ 2 ];
        m_selectedUIElement->GetPointIntersectionInPixels( x, y, m_intersectionPoint );
        //std::cout << x << " " << trans.x() << " " << y << " " << trans.y() << std::endl;
        // Flip y mouse coordinate to origin GUI expects
        y = static_cast < double > ( mTop ) - y;
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
    if( ! _okayToSendEvent() )
    {
        return false;
    }

    // Intercept the UI hide/show hotkey.
    if( key == gadget::KEY_F1 )
    {
        // Do nothing here; the work is done in KeyReleaseEvent
        return true;
    }

    // TODO: this iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
    ElementMap_type::const_iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        UIElement* element = map_iterator->second;

        bool visible = element->IsVisible();
        bool minimized = element->IsMinimized();
        // Only send events if element is visible and not minimzed
        if( ( visible ) && ( !minimized ) )
        {
            element->SendKeyPressEvent( key, modifiers, unicode );
        }
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

    // TODO: this iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
    ElementMap_type::const_iterator map_iterator;
    for( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        UIElement* element = map_iterator->second;

        bool visible = element->IsVisible();
        bool minimized = element->IsMinimized();

        // Only send events if element is visible and not minimzed
        if( ( visible ) && ( !minimized ) )
        {
            element->SendKeyReleaseEvent( key, modifiers, unicode );
        }
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
    //Get line from user pressing button 0
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
        //We are over the UI somewhere
        mMouseInsideUI = true;
        
        //Now do a test to determine where the wand ray is interesting the plane
        //of the UI texture so that we can translate that to an x,y location
        osgUtil::LineSegmentIntersector::Intersection tempIntersection = 
            *(intersections.begin());
        m_intersectionPoint = tempIntersection.getLocalIntersectPoint();
        
        m_selectedUINode = *(tempIntersection.nodePath.rbegin());
        std::cout << "Wand intersection at " << m_intersectionPoint 
            << " with the this UI node "
            << m_selectedUINode->getName() << std::endl;
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
