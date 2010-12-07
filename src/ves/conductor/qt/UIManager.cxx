/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

// --- OSG Includes --- //
//#include <osg/Geometry>
#include <osg/Group>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/StateAttribute>
//#include <osg/Texture2D>
#include <osg/Projection>
#include <osg/NodeCallback>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/AnimationPath>
#include <osg/io_utils>

// --- STL Includes --- //
#include <iostream>

//#define VES_QT_RENDER_DEBUG

using namespace ves::conductor;

vprSingletonImp( UIManager );

////////////////////////////////////////////////////////////////////////////////
UIManager::UIManager() :
    mUIUpdateCallback( new UIUpdateCallback ),
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
    mUnminimizeElement( 0 )
{    
    CONNECTSIGNALS_0( "%HideShowUI%", void (), &UIManager::ToggleVisibility, mConnections,
                      any_SignalType, highest_Priority);

    CONNECTSIGNAL_4( "KeyboardMouse.MouseMove", void ( int, int, int, int ),
                     &UIManager::MouseMoveEvent, mConnections, highest_Priority );

    CONNECTSIGNALS_4( "%ButtonPress%",void ( gadget::Keys, int, int, int ),
                      &UIManager::ButtonPressEvent, mConnections,
                      button_SignalType, highest_Priority );

    CONNECTSIGNALS_4( "%ButtonRelease%",void ( gadget::Keys, int, int, int ),
                      &UIManager::ButtonReleaseEvent, mConnections,
                      button_SignalType, highest_Priority );

    CONNECTSIGNALS_3( "%KeyPress%", void ( gadget::Keys, int, wchar_t ),
                      &UIManager::KeyPressEvent, mConnections,
                      keyboard_SignalType, highest_Priority );

    CONNECTSIGNALS_3( "%KeyRelease%", void ( gadget::Keys, int, wchar_t ),
                      &UIManager::KeyReleaseEvent, mConnections,
                      keyboard_SignalType, highest_Priority );

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
    ElementMap_type::iterator map_iterator;
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
    mNodesToAdd.push_back( element->GetVisibilitySwitch() );

    mElementPositionsOrtho2D[ element ] = _computeMouseBoundsForElement( element );

    osg::Geode* geode = element->GetGeode();
    mElements[ geode ] = element;

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::RemoveElement( osg::ref_ptr<osg::Geode> geode )
{
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

    // Update the UI's rectangle if it has been dirtied.
    if( mRectangleDirty )
    {
        mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
                                                      mBottom, mTop ) );
        //mProjection->setMatrix( mTempProj );
        mRectangleDirty = false;
    }

    // Do hide/show operations
    if( mToggleVisibility )
    {
        if( mUIGroup->asSwitch()->getValue( 0 ) )
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
        //mOverallOpacity->setAlpha( osg::Material::FRONT_AND_BACK, mOpacity );
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

    mProjection = new osg::Projection();
    mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
                                                  mBottom, mTop ) );

    osg::MatrixTransform* modelViewMatrix = new osg::MatrixTransform;
    modelViewMatrix->setMatrix( osg::Matrix::identity() );
    modelViewMatrix->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

    mProjection->addChild( modelViewMatrix );

    mUIGroup = new osg::Switch();
    mUIGroup->setDataVariance( osg::Object::DYNAMIC );
    mUIGroup->setUpdateCallback( mUIUpdateCallback.get() );
    modelViewMatrix->addChild( mUIGroup.get() );

    parentNode->addChild( mProjection.get() );

    //Vertex shader not used yet
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
    vertexShader->setName( "VS Quad Vertex Shader" );

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
            "uniform sampler2D baseMap; \n"
            "uniform float opacityVal;\n"
            "uniform vec3 glowColor; \n"

            "void main() \n"
            "{ \n"
            "vec4 baseColor = texture2D( baseMap, gl_TexCoord[ 0 ].st ); \n"
            "baseColor.a = opacityVal;\n"
            "gl_FragData[ 0 ] = baseColor; \n"
            "gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a ); \n"
            "} \n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS Quad Fragment Shader" );

    //
    osg::ref_ptr< osg::Program > program = new osg::Program();
    //program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    program->setName( "VS Quad Program" );

    //Set depth test to always pass and don't write to the depth buffer
    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    //depth->setFunction( osg::Depth::ALWAYS );
    depth->setWriteMask( false );

    //Create stateset for adding texture
    osg::ref_ptr< osg::StateSet > stateset = mUIGroup->getOrCreateStateSet();
    stateset->setRenderBinDetails( 99, "RenderBin" );
    stateset->setAttributeAndModes(
                                    depth.get(),
                                    osg::StateAttribute::ON | osg::StateAttribute::PROTECTED |
                                    osg::StateAttribute::OVERRIDE );
    stateset->setMode( GL_DEPTH_TEST, osg::StateAttribute::OFF );
    stateset->setMode(
                       GL_LIGHTING,
                       osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED |
                       osg::StateAttribute::OVERRIDE );
    stateset->setAttributeAndModes(
                                    program.get(),
                                    osg::StateAttribute::ON | osg::StateAttribute::PROTECTED |
                                    osg::StateAttribute::OVERRIDE );
    stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );
    m_opacityUniform = new osg::Uniform( "opacityVal", mOpacity );
    stateset->addUniform( m_opacityUniform.get() );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                    osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
    stateset->setMode( GL_BLEND, 
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes( bf.get(), 
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    mInitialized = true;
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent &event )
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
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_insertNodesToAdd()
{
    std::vector< osg::ref_ptr<osg::Switch> >::iterator vec_iterator;
    for( vec_iterator = mNodesToAdd.begin();
            vec_iterator != mNodesToAdd.end();
            ++vec_iterator )
    {
        mUIGroup->addChild( ( *vec_iterator ).get() );
        ( *vec_iterator ).release();
    }

    mNodesToAdd.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_repaintChildren()
{
    ElementMap_type::iterator map_iterator;
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
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_showAll()
{
    //if( !showOnlyActive )
    {
        ElementMap_type::iterator map_iterator;
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
void UIManager::SetProjectionMatrix( osg::Matrixd& matrix )
{
    if( matrix != mTempProj )
    {
        mRectangleDirty = true;
        mTempProj = matrix;
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::UnembedAll()
{
    //    if( !mInitialized )
    //    {
    //        return;
    //    }
    //
    //    HideAllElements();
    //    ElementMap_type::iterator map_iterator;
    //    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
    //            ++map_iterator )
    //    {
    //        UIElement* element = map_iterator->second;
    //        element->Unembed();
    //    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::EmbedAll()
{
    //    if( !mInitialized )
    //    {
    //        return;
    //    }
    //
    //    ElementMap_type::iterator map_iterator;
    //    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
    //            ++map_iterator )
    //    {
    //        UIElement* element = map_iterator->second;
    //        element->Embed();
    //    }
    //    ShowAllElements();
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::Ortho2DTestPointerCoordinates( int x, int y )
{
    // Walk through every quad we own and see if the point lies on it
    osg::Vec4 quadPos;
    std::map< UIElement*, osg::Vec4 >::iterator map_iterator = mElementPositionsOrtho2D.begin();
    while( map_iterator != mElementPositionsOrtho2D.end() )
    {
        quadPos = map_iterator->second;
        /*std::cout << "Testing (" << x << ", " << y << ") against ("
                << quadPos.x() << ", " << quadPos.y() << ", " << quadPos.z()
                << ", " << quadPos.w() << ")\n";*/
        if( ( x >= quadPos.x() ) && ( x <= quadPos.y() ) &&
                ( y >= quadPos.z() ) && ( y <= quadPos.w() ) )
        {
            return true;
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
        ElementMap_type::iterator map_iterator;
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
    float duration = 0.4f;

    // Animation based on two control points: c0 (current state) and c1 (end state)
    osg::Matrixf currentMatrix = element->GetUIMatrix();

    osg::AnimationPath::ControlPoint c0( currentMatrix.getTrans() );
    c0.setScale( currentMatrix.getScale() );

    osg::AnimationPath::ControlPoint c1;

    // Set up the second animation control point based on whether this is a
    // min or an unmin operation. Also set the element's UIMatrix to match
    // the end state of the animation.
    if( minimize )
    {
        element->SetMinimized( true );
        c1.setPosition( osg::Vec3f( mMinimizeXOffset, yPadding, 0.0f ) );
        c1.setScale( osg::Vec3f( downScale, downScale, downScale ) );
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
        element->PopUIMatrix();
        c1.setPosition( element->GetUIMatrix().getTrans() );
        c1.setScale( element->GetUIMatrix().getScale() );
    }

    osg::ref_ptr< osg::AnimationPath > path = new osg::AnimationPath;
    path->setLoopMode( osg::AnimationPath::NO_LOOPING );

    path->insert( 0.0f, c0 );
    path->insert( duration, c1 );

    element->SetAnimationPath( path.get() );

    mElementPositionsOrtho2D[ element ] = _computeMouseBoundsForElement( element );
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
        ElementMap_type::iterator map_iterator;
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
    osg::Matrixf fullTransform = element->GetElementMatrix() * element->GetUIMatrix();

    // Compute transformed corner coordinates of a unit square with origin at
    // (0,0,0)
    osg::Vec4 bl = osg::Vec4( 0.f, 0.f, 0.f, 1.f ) * fullTransform;
    osg::Vec4 tr = osg::Vec4( 1.f, 1.f, 0.f, 1.f ) * fullTransform;

    // Return in the form (left, right, bottom, top)
    return osg::Vec4( bl.x(), tr.x(), bl.y(), tr.y() );
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
void UIManager::ButtonPressEvent( gadget::Keys button, int x, int y, int state )
{
    if( !_okayToSendEvent() )
    {
        return;
    }

    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return;
    }

    // TODO: his iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
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
            x = x - trans.x();
            y = y - trans.y();

            // Flip y mouse coordinate to origin GUI expects
            y = static_cast < double > ( mTop ) - y;
            element->SendButtonPressEvent( button, x, y, state );
        }
        else
        {
            mUnminimize = true;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ButtonReleaseEvent( gadget::Keys button, int x, int y, int state )
{
    if( !_okayToSendEvent() )
    {
        return;
    }

    // Store off coordinates and deltas
    mDxPointer = x - mCurrentXPointer;
    mDyPointer = y - mCurrentYPointer;
    mCurrentXPointer = x;
    mCurrentYPointer = y;

    // If we're ending an element move, do that and sink the event
    if( mMoveElement )
    {
        mElementPositionsOrtho2D[ mMoveElement ] = _computeMouseBoundsForElement( mMoveElement );
        mMoveElement = 0;
        return;
    }

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return;
    }

    // TODO: this iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
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
            x = x - trans.x();
            y = y - trans.y();

            // Flip y mouse coordinate to origin GUI expects
            y = static_cast < double > ( mTop ) - y;
            element->SendButtonReleaseEvent( button, x, y, state );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::MouseMoveEvent( int x, int y, int z, int state )
{

    if( !_okayToSendEvent() )
    {
        return;
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
        return;
    }

    if( !Ortho2DTestPointerCoordinates( x, y ) )
    {
        return;
    }

    // TODO: this iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
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
            x = x - trans.x();
            y = y - trans.y();

            // Flip y mouse coordinate to origin GUI expects
            y = static_cast < double > ( mTop ) - y;
            element->SendMouseMoveEvent( x, y, z, state );
        }
    }

}
////////////////////////////////////////////////////////////////////////////////
void UIManager::KeyPressEvent( gadget::Keys key, int modifiers, wchar_t unicode )
{
    if( ! _okayToSendEvent() )
    {
        return;
    }

    // TODO: this iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
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
            element->SendKeyPressEvent( key, modifiers, unicode );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::KeyReleaseEvent( gadget::Keys key, int modifiers, wchar_t unicode )
{
    if( ! _okayToSendEvent() )
    {
        return;
    }

    // TODO: this iterates over all elements. We should instead just find the match
    // from Ortho2DTestPointerCoordinates and send to it.
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
            element->SendKeyReleaseEvent( key, modifiers, unicode );
        }
    }
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
