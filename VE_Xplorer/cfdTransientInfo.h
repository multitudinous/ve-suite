/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdTransientInfo.h,v $
 * Date modified: $Date: 2004/03/23 16:29:19 $
 * Version:       $Revision: 1.4 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TRANSIENT_INFO_H
#define CFD_TRANSIENT_INFO_H

#include <vector>

class pfDCS;
class cfdTransientSet;

class cfdTransientInfo
{
   public:
      cfdTransientInfo();
      ~cfdTransientInfo();

      // get/set this dataset's DCS
      pfDCS * GetDCS();
      void    SetDCS( pfDCS * );

      // get/set this geometry's DCS
      pfDCS * GetGeometryDCS();
      void    SetGeometryDCS( pfDCS * );

      char  * GetGeometryDir();
      void    SetGeometryDir( char * );

      void LoadTransientSet( cfdTransientSet * );
      int GetNumberOfTransSets();
      cfdTransientSet * GetTransSet( int index );
      cfdTransientSet * GetFlowdataTransSet();
      cfdTransientSet * Get_X_planeTransSet();
      cfdTransientSet * Get_Y_planeTransSet();
      cfdTransientSet * Get_Z_planeTransSet();
      cfdTransientSet * GetDropletTransSet();

      //the display duration of the entire animated sequence
      double  GetDuration();
      void    SetDuration( double );

      int     trans;          // transparency flag
      int     color;          // is color specified ?
      float   stlColor[ 3 ];  // rgb color ( 0 < stlColor < 1 )

   private:
      std::vector< cfdTransientSet * > transientSet;
      cfdTransientSet * findTransientSetWithID( int );
      pfDCS * dcs;
      char  * geometryDir;
      pfDCS * geometryDcs;
      double  duration;
      int     numberOfFrames;
};

#endif
