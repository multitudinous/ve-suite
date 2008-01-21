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
#ifndef NODE
#define NODE

#include <cmath>

class Node
{
public:
  Node( );
  ~Node( );

  void SetParent( Node *parentNode ) { itsParent = parentNode; }
  Node * GetParent() const { return itsParent; }

  void SetLevel( int level ) { itsLevel = level; }
  int GetLevel() const { return itsLevel; }

  void SetID( int ID ) { itsID = ID; }
  int GetID() const { return itsID; }

  void SetProperties( Octant *octant ) { itsOctant = octant; }

  Octant * GetOctant() const { return itsOctant; }

  void Insert( Node * );

  Node * GetNode() const { return itsNext; };

  int GetNumberOfNodesAt_h_Height( int height );

private:
  Octant *itsOctant;
  Node *itsParent;
  Node * itsNext;
  int itsLevel;
  int itsID;
};

Node::Node( )
{
  itsOctant = 0;
  itsParent = 0;
  itsNext = 0;
  itsLevel = 0;
  itsID = 0;
}

Node::~Node()
{
//   cout << "Deleting node... \n";
  delete itsOctant;
  itsOctant = 0;
  delete itsNext;
  itsNext = 0;
}

void Node::Insert( Node * newNode )
{
  if( !itsNext )
    {
      itsNext = newNode;
    }
  else
    {
      itsNext->Insert( newNode );
    }
}

int Node::GetNumberOfNodesAt_h_Height( int height )
{
  int totalNumberOfNodes;
  int degree = 8;
  totalNumberOfNodes = ( (int)pow( (double)degree, (double)( height + 1 ) ) - 1 ) / ( degree - 1);
  return totalNumberOfNodes;
}
#endif
