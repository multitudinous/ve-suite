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
#ifndef OCTANT
#define OCTANT
#include <iostream>

class Octant
{
public:
  Octant( );
  ~Octant( ){ };

  void SetNumberOfCells( int cellNo ) { itsCellNo = cellNo; }
  int GetNumberOfCells() const { return itsCellNo; }
  void SetBound( double bound[6] );
  void GetBound( double bound[6] );
  void GetOctantsPoints( double bound[6], float xout[27], float yout[27], float zout[27] );
  void GetSiblingBound( int siblingID, float xout[], float yout[], float zout[], double bound[6] );

private:
  int itsCellNo;
  float itsbound[6];
};

Octant::Octant()
{
  itsCellNo = 0;

  for ( int i=0; i<6; i++ )
  {
      itsbound[i] = 0.0f;
  }
}

void Octant::SetBound( double bound[6] )
{
  for ( int i=0; i<6; i++ )
        itsbound[i] = (float)bound[i];
}

void Octant::GetBound( double bound[6] )
{
  for ( int i=0; i<6; i++ )
        bound[i] = (double)itsbound[i];
}

void Octant::GetOctantsPoints( double bound[6], 
			       float xout[27], float yout[27], float zout[27] )
{
  int i;
  int j = 0;
  float dx, dy, dz;
  float z = 0.0f;

  dx = (float)(bound[1] - bound[0]);
  dy = (float)(bound[3] - bound[2]);
  dz = (float)(bound[5] - bound[4]);

  for( i=0; i<3; i++ )
     {
       xout[j+0] = static_cast<float>(bound[0]);           yout[j+0] = static_cast<float>(bound[2]);           zout[j+0] = static_cast<float>(bound[4]) + z;
       xout[j+1] = static_cast<float>(bound[0]);           yout[j+1] = static_cast<float>(bound[2]) + dy*0.5f; zout[j+1] = static_cast<float>(bound[4]) + z;
       xout[j+2] = static_cast<float>(bound[0]);           yout[j+2] = static_cast<float>(bound[2]) + dy;      zout[j+2] = static_cast<float>(bound[4]) + z;
       xout[j+3] = static_cast<float>(bound[0]) + dx*0.5f; yout[j+3] = static_cast<float>(bound[2]);           zout[j+3] = static_cast<float>(bound[4]) + z;
       xout[j+4] = static_cast<float>(bound[0]) + dx*0.5f; yout[j+4] = static_cast<float>(bound[2]) + dy*0.5f; zout[j+4] = static_cast<float>(bound[4]) + z;
       xout[j+5] = static_cast<float>(bound[0]) + dx*0.5f; yout[j+5] = static_cast<float>(bound[2]) + dy;      zout[j+5] = static_cast<float>(bound[4]) + z;
       xout[j+6] = static_cast<float>(bound[0]) + dx;      yout[j+6] = static_cast<float>(bound[2]);           zout[j+6] = static_cast<float>(bound[4]) + z;
       xout[j+7] = static_cast<float>(bound[0]) + dx;      yout[j+7] = static_cast<float>(bound[2])+ dy*0.5f;  zout[j+7] = static_cast<float>(bound[4]) + z;
       xout[j+8] = static_cast<float>(bound[0]) + dx;      yout[j+8] = static_cast<float>(bound[2]) + dy;      zout[j+8] = static_cast<float>(bound[4]) + z;

       j += 9;
       z += dz*0.5f;
     }
}

void Octant::GetSiblingBound( int siblingID, float xout[], float yout[], float zout[],
			      double bound[6] )
{
  switch ( siblingID )
    {
    case 0:
      bound[0] = (double)xout[0];
      bound[1] = (double)xout[13];
      bound[2] = (double)yout[0];
      bound[3] = (double)yout[13];
      bound[4] = (double)zout[0];
      bound[5] = (double)zout[13];
    break;
    case 1:
      bound[0] = (double)xout[1];
      bound[1] = (double)xout[14];
      bound[2] = (double)yout[1];
      bound[3] = (double)yout[14];
      bound[4] = (double)zout[1];
      bound[5] = (double)zout[14];
    break;
    case 2:
      bound[0] = (double)xout[3];
      bound[1] = (double)xout[16];
      bound[2] = (double)yout[3];
      bound[3] = (double)yout[16];
      bound[4] = (double)zout[3];
      bound[5] = (double)zout[16];
    break;
    case 3:
      bound[0] = (double)xout[4];
      bound[1] = (double)xout[17];
      bound[2] = (double)yout[4];
      bound[3] = (double)yout[17];
      bound[4] = (double)zout[4];
      bound[5] = (double)zout[17];
    break;
    case 4:
      bound[0] = (double)xout[9];
      bound[1] = (double)xout[22];
      bound[2] = (double)yout[9];
      bound[3] = (double)yout[22];
      bound[4] = (double)zout[9];
      bound[5] = (double)zout[22];
    break;
    case 5:
      bound[0] = (double)xout[10];
      bound[1] = (double)xout[23];
      bound[2] = (double)yout[10];
      bound[3] = (double)yout[23];
      bound[4] = (double)zout[10];
      bound[5] = (double)zout[23];
    break;
    case 6:
      bound[0] = (double)xout[12];
      bound[1] = (double)xout[25];
      bound[2] = (double)yout[12];
      bound[3] = (double)yout[25];
      bound[4] = (double)zout[12];
      bound[5] = (double)zout[25];
    break;
    case 7:
      bound[0] = (double)xout[13];
      bound[1] = (double)xout[26];
      bound[2] = (double)yout[13];
      bound[3] = (double)yout[26];
      bound[4] = (double)zout[13];
      bound[5] = (double)zout[26];
    break;
    default: std::cout << "Error no selection" << std::endl;
    break;
    }
}
#endif
