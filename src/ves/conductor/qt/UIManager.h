/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef VES_CONDUCTOR_UI_MANAGER_H
#define VES_CONDUCTOR_UI_MANAGER_H

// --- VES Includes --- //
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>
#include <ves/VEConfig.h>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Matrixd>

namespace osg
{
class Group;
class Switch;
class MatrixTransform;
class Geode;
class StateAttribute;
//class Texture2D;
class Projection;
class NodeCallback;
//class Material;
}

// --- STL includes --- //
#include <string>
#include <map>

namespace ves
{
namespace xplorer
{
namespace eventmanager
{
class InteractionEvent;
}
}

namespace conductor
{
class UIElement;
class UIUpdateCallback;

/*!\file UIManager.h
 *
 */

/*!\class ves::conductor::UIManager
 *
 */

/*!\namespace ves::conductor
 *
 */
/// Manages UIElements by controlling element focus, passing interaction events,
/// requesting element repaints, toggling drawing of elements, ?? MORE ??
/// NOTE: State of scenegraph and traversal is only important during call to
/// Update.
class VE_CONDUCTOR_QTUI_EXPORTS UIManager : public boost::noncopyable
{
public:
    /**
     * Adds a UIElement to be managed.
     *
     * !!! CRITICAL !!! UIManager henceforth assumes that it *owns* the UIElement.
     * UIManager will call delete on the UIElement during a call to RemoveElement
     * and during destruction of UIManager. Once a UIElement is handed to UIManager,
     * you may retain a pointer to programatically interact with it, if you like,
     * but DO NOT CALL DELETE ON THE UIELEMENT.
     *
     * Internally, the UIManager owns a group called UIGroup in the scenegraph.
     * When an element is added, a new sub-graph that looks like the following
     * is added as a child to UIGroup:
     *
     * Switch
     *    |-- MatrixTransform
     *              |-- Geode
     **/
    osg::Geode* AddElement( UIElement* element );

    /// Removes element associated with geode from our management.
    /// Discards UIManager's ref_ptr(s) to everything associated with this geode,
    /// and calls delete on the associated UIElement.
    bool RemoveElement( osg::ref_ptr<osg::Geode> geode );

    // TODO document RemoveAllElements
    ///
    void RemoveAllElements( );

    /// Do all needed updates on the scenegraph.
    /// Any UIElements added to the manager since the last call to update will
    /// be added to the scenegraph, and all UIElements that are currently enabled
    /// will be given a chance to redraw. This does not normally need to be called
    /// directly, since this class sets up its own update callback with OSG and
    /// will do its updates automatically at the appropriate time. This method
    /// is made public strictly for those cases in which you need to ensure that
    /// UI updates happen at a specific time during the scenegraph updates rather
    /// than whenever the update traversal actually gets to the root node of the
    /// UI branch. These instances should be very rare, so if you find yourself
    /// calling this method, make sure you know what you are doing.
    ///
    /// !!! CRITICAL !!!
    /// This function should *only* be called during the update traversal of the
    /// scenegraph. Calling it any other time may cause ref_ptrs to go out of scope
    /// at the wrong time and result in loss of geometry.
    void Update();

    ///Hide all UI elements
    void HideAllElements();

    ///Show all UI elements
    void ShowAllElements( bool showOnlyActive = true );

    /// Toggle visibility of all UI elements; that is, hide the entire UI branch
    /// if it is currently visible, and show it if it is currently hidden.
    void ToggleVisibility();

    ///Set the bounds of the projection rectangle for Ortho2D mode
    void SetRectangle( int left, int right, int bottom, int top );

    /// Tell UIManager to set up its initial subtree in the scenegraph and
    /// register its update callback with OSG.
    ///
    /// !!! CRITICAL !!!
    /// This method *must* be called during an update traversal, since nodes
    /// are added to the scenegraph inside this method.
    void Initialize( osg::Group* parentNode );

    ///Propagates mouse and keyboard events to elements
    bool SendInteractionEvent( xplorer::eventmanager::InteractionEvent& event );

    ///Sets the projection matrix when not in Ortho2D mode
    void SetProjectionMatrix( osg::Matrixd& matrix );

    /// Unimplemented
    void UnembedAll();

    /// Unimplemented
    void EmbedAll();

    // Returns true if the point (x,y) is over a quad associated with a UIElement,
    // false otherwise.
    bool Ortho2DTestPointerCoordinates( int x, int y );

    /// Set the overall UI opacity
    void SetOpacity( float opacity );

    /// Minimize all elements to bottom of screen
    void MinimizeAllElements();

    /// Let UIManager know that we should begin moving element. This will usually
    /// be called after a click on an element's titlebar or some similar operation.
    void InitiateMoveElement( UIElement* element );

    /// Minimize only the specific element passed as the argument
    void MinimizeElement( UIElement* element );

    /// Unminimize the element passed as the argument
    void UnminimizeElement( UIElement* element );

    /// Hide only the element passed as the argument
    void HideElement( UIElement* element );

    /// Unhide the element passed as the argument
    void ShowElement( UIElement* element );

    /// Hide if shown or show is hidden the element passed as the argument
    void ToggleElementVisibility( UIElement* element );

private:
    // Set this class up as a singleton
    ///Constructor
    UIManager();

    ///Destructor
    virtual ~UIManager();

    /// Singleton declarations
    vprSingletonHeader( UIManager );

    // NodeCallback as a member object rather than via inheritance so as not to
    // break singleton pattern.
    osg::ref_ptr< osg::NodeCallback > mUIUpdateCallback;

    /// Stores the UIElements in key/pair form.
    /// The key is an osg::Geode node containing the geometry used to display the element
    /// The pair is a pointer to the actual UIElement.
    typedef std::map< osg::ref_ptr< osg::Geode >, UIElement* > ElementMap_type;
    ElementMap_type mElements;

    /// Stores nodes that should be added to scenegraph during next update
    std::vector< osg::ref_ptr< osg::Switch > > mNodesToAdd;

    /// Root node of the UI branch
    osg::ref_ptr< osg::Switch > mUIGroup;

    /// Flag to tell if we're initialized
    bool mInitialized;

    /// Projection matrix that applies to everything under mUIGroup. This is
    /// how we deal with Ortho projection and/or head tracking
    osg::ref_ptr< osg::Projection > mProjection;
    //osg::ref_ptr< osg::MatrixTransform > mProjection;
    osg::Matrixd mTempProj;

    /// Left-most coordinate in ortho view
    int mLeft;

    /// Right-most coordinate in ortho view
    int mRight;

    /// Bottom-most coordinate in ortho view
    int mBottom;

    /// Top-most coordinate in ortho view
    int mTop;

    /// Flag to tell if the rectangle describing ortho view has been altered
    /// since last update
    bool mRectangleDirty;

    /// Flag set when overall visibility has been toggled
    bool mToggleVisibility;

    /// Flag set when HideAllElements has been called
    bool mHide;

    /// Flag set when ShowAllElements has been called
    bool mShow;

    bool mMinimize;

    bool mUnminimize;

    /// Pointer directly to the material controlling overall opacity of all
    /// UI Elements
    //osg::ref_ptr< osg::Material > mOverallOpacity;

    /// Current value of overall opacity
    float mOpacity;

    int mDxPointer;
    int mDyPointer;
    int mDzPointer;

    int mCurrentXPointer;
    int mCurrentYPointer;
    int mCurrentZPointer;

    float mMinimizeXOffset;

    UIElement* mMoveElement;
    UIElement* mMinimizeElement;
    UIElement* mUnminimizeElement;

    /// Helper function to add in nodes during update if necessary
    void _insertNodesToAdd();

    /// Helper function to get new texture images for elements when needed
    void _repaintChildren();

    /// Helper function to send out events to elements
    void _sendEvent();

    /// Helper function to hide elements
    void _hideAll();

    /// Helper function to show elements
    void _showAll();

    void _doMinimize();

    void _doUnminimize();

    void _doMinMaxElement( UIElement* element, bool minimize );

    osg::Vec4 _computeMouseBoundsForElement( UIElement* element );

    ///
    ves::xplorer::eventmanager::ScopedConnectionList mConnections;

    ///
    std::map< UIElement*, osg::Vec4 > mElementPositionsOrtho2D;
};

} //end conductor
} //end ves

#endif //VES_CONDUCTOR_UI_MANAGER_H
