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
#ifndef DCS_H
#define DCS_H

/*!\file DCS.h
 */

/*!\class ves::xplorer::scenegraph::DCS
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneNode.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class TransferPhysicsDataCallback;
}
}
}

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/PositionAttitudeTransform>
#elif _OPENSG
#endif

// --- VR Juggler Includes --- //
#include <gmtl/Matrix.h>
#include <gmtl/Quat.h>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/range_c.hpp>
#include <boost/lambda/lambda.hpp>

// --- Bullet Includes --- //
class btRigidBody;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

template< typename DATA_TYPE_OUT, typename DATA_TYPE_IN, unsigned ROWS, unsigned COLS >
gmtl::Matrix< DATA_TYPE_OUT, ROWS, COLS >
convertTo( const gmtl::Matrix< DATA_TYPE_IN, ROWS, COLS >& in )
{
    using namespace boost::lambda;

    gmtl::Matrix< DATA_TYPE_OUT, ROWS, COLS > out;

    const DATA_TYPE_IN* in_data( in.mData );
    DATA_TYPE_OUT* out_data( out.mData );

    boost::mpl::for_each< boost::mpl::range_c< unsigned int, 0, ROWS * COLS> >
    ( *( out_data + _1 ) = *( in_data + _1 ) );

    return out;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsRigidBody;

#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS DCS : public osg::PositionAttitudeTransform, public SceneNode
#elif _OPENSG
#endif
{
public:
    ///Constructor
    DCS();

protected:
    ///Destructor
    virtual ~DCS();

public:
    ///Constructor to set user defined transform values
    ///\param scale The scale array pointer
    ///\param trans The translation array pointer
    ///\param rot The rotation array pointer
    DCS( double* scale, double* trans, double* rot );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    DCS( const DCS&, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( ves::xplorer::scenegraph, DCS );

    ///Get translation array pointer
    double* GetVETranslationArray();

    ///Get rotation array pointer
    double* GetRotationArray();

    ///Get scale array pointer
    double* GetScaleArray();

    ///Get the transform in 4x4 matrix form
    gmtl::Matrix44d GetMat();

    ///Set the translation array with a vector
    ///\param transArray A vector to set the translation array values
    void SetTranslationArray( std::vector< double > transArray );

    ///Set the translation array
    ///\param trans A pointer to set the translation array values
    void SetTranslationArray( double* trans );

    ///Set the attitude with a quat
    ///\param quat A quat to set the attitude
    void SetQuat( osg::Quat quat );

    ///Set the rotation array with a vector
    ///\param rotArray A vector to set the rotation array values
    void SetRotationArray( std::vector< double > rotArray );

    ///Set the rotation array with a pointer
    ///\param rot A pointer to set the rotation array values
    void SetRotationArray( double* rot );

    ///Set the scale array with a vector
    ///\param scaleArray A vector to set the scale array values
    void SetScaleArray( std::vector< double > scaleArray );

    ///Set the scale array with a pointer
    ///\param scale A pointer to set the scale array values
    void SetScaleArray( double* scale );

    ///Set the matrix for this transform
    ///\param input Matrix with all transform information
    void SetMat( gmtl::Matrix44d& input );

    ///Set just the roation portion of this transform with the 4x4 matrix
    ///\param input Matrix with pure rotations
    void SetRotationMatrix( gmtl::Matrix44d& input );

    ///Generic set name function
    ///\param name The name
    void SetName( std::string name );

    ///Generic remove child function
    ///\param child The child to be removed
    int RemoveChild( SceneNode* child );

    ///Generic add child function
    ///\param child The child to be added
    int AddChild( SceneNode* child );

    ///Generic insert child function
    ///\param position The position of the child to be inserted
    ///\param child The child that is inserted
    void InsertChild( int position, SceneNode* child );

    ///Generic replace child function
    ///\param childToBeReplaced The child to be replaced
    ///\param newChild The new child
    int ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild );

    ///Generic search child function
    ///\param searchChild SceneNode* of child to be found
    bool SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild );

    ///Generic find parent function
    ///\param position The position of the parent to be returned
    osg::Group* GetParent( unsigned int position );

    ///Generic get child function
    ///\param position The position of the child to be returned
    osg::Node* GetChild( unsigned int position );

    ///Generic get number of children
    int GetNumChildren();

    ///Set the name of the node
    const std::string GetName();

    ///Toggle the display of this node
    ///\param onOff Toggle the display of this node
    void ToggleDisplay( std::string onOff );

    ///Toggle the display of this node
    ///\param onOff Toggle the display of this node
    void ToggleDisplay( bool onOff );

    ///Set the Bullet rigid body for this node, typically is set from CADEntity
    ///\param rigidBody The btRigidBody*
    void SetPhysicsRigidBody( PhysicsRigidBody* physicsRigidBody );

protected:
    double m_Rotation[ 3 ];///<The rotation array
    double m_Translation[ 3 ];///<The translation array
    double m_Scale[ 3 ];///<The scale array

    PhysicsRigidBody* m_physicsRigidBody;///<The rigid body to access the respective btTransform

private:
    ///Update the bullet matrix with the matrix from the osg node
    ///The osg node gets set first and is then updated by the bullet physics simulator
    void UpdatePhysicsTransform();

    osg::ref_ptr< TransferPhysicsDataCallback > m_udcb;///<The callback to update the sg node with physics data

// -------------------------------------------------- //
// --- This stuff is used for multipass rendering --- //
// -------------------------------------------------- //
public:
    virtual void traverse( osg::NodeVisitor& nv );
    virtual void InheritedTraverse( osg::NodeVisitor& nv );

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //DCS_H
