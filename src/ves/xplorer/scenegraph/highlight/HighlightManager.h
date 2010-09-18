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

#ifndef VES_XPLORER_SCENEGRAPH_HIGHLIGHT_HIGHLIGHTMANAGER_H
#define VES_XPLORER_SCENEGRAPH_HIGHLIGHT_HIGHLIGHTMANAGER_H

// --- VES Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/Group>

// --- STL Includes --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace highlight
{
class CircleHighlight;

/*!\file HighlightManager.h
 * HighlightManager API
 */

/*!\class ves::xplorer::scenegraph::highligh::HighlightManager
 *
 */
class VE_SCENEGRAPH_EXPORTS HighlightManager : public osg::Group
{
public:
    ///Constructor
    HighlightManager();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    HighlightManager(
        const HighlightManager& highlightManager,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::highlight, HighlightManager );

    ///Override the addChild function to only accept Highlights
    virtual bool addChild( CircleHighlight* child );

    ///
    CircleHighlight* const ConvertNodeToCircleHighlight(
        osg::Node* const node );

    ///
    void Enable( const bool& enable = true );

    ///
    CircleHighlight* const GetActiveCircleHighlight() const;

    ///Override the insertChild function to only accept Highlights
    virtual bool insertChild( unsigned int index, CircleHighlight* child );

    ///
    const bool IsEnabled() const;

    ///
    bool const& IsToggled() const;

    ///
    virtual void removeChildren();

    ///Override the replaceChild function to only accept Highlights
    virtual bool replaceChild(
        CircleHighlight* origChild, CircleHighlight* newChild );

    ///
    void SetActiveCircleHighlight( CircleHighlight* circleHighlight );

    ///Override the setChild function to only accept Highlights
    virtual bool setChild( unsigned int i, CircleHighlight* node );

    ///
    void Toggle( bool const& toggle = true );

    ///Return an incremented tag name
    const std::string& GetNextTagName();

    ///Set the node this circle highlight is associated with
    bool IsNodeCircled( osg::Node* inNode );
    
    ///
    void RegisterNodeAndHighlight( osg::Node* inNode, CircleHighlight* circle );

    ///Create a new highlight circle for a given node path and node
    void CreateHighlightCircle( osg::Node* inNode, osg::NodePath& nodePath );
protected:
    ///Destructor
    virtual ~HighlightManager();

private:
    ///
    void UpdateConductorData();

    ///Is the highlight manager turned on
    bool m_enabled;

    ///
    bool m_toggled;

    ///The active highlight
    CircleHighlight* m_activeCircleHighlight;

    ///Store tag names
    std::vector< std::string > m_tagNames;
    
    ///Store nodes associated with highlight circles
    std::map< osg::Node*, CircleHighlight* > m_nodeToCircleHighlights;
    typedef std::map< osg::Node*, CircleHighlight* >::const_iterator 
        NodeToCircleHighlightsIter;
};
} //end highlight
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_HIGHLIGHT_HIGHLIGHTMANAGER_H
