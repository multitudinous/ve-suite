/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdStateInfo.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <vpr/IO/SerializableObject.h>

namespace ClusterVariables
{
struct StateVariables
{
   int   clusterIso_value;
   int   clusterSc;
   int   clusterMin;
   int   clusterMax;
   long  clusterId;
   long  clusterGeo_state;
   short clusterPostdata_state;
   bool  clusterPre_state;
   short clusterTimesteps;
   short clusterTeacher_state; 
   short clusterClientInfoFlag; 
   int currentFrame; // the index of the current frame
   float clusterTime_since_start;
   long clusterFrameNumber;
};
}

namespace vpr
{
template<>
inline vpr::ReturnStatus vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::writeObject(vpr::ObjectWriter* writer)
{ 
   writer->writeFloat( (float)clusterIso_value );  
   writer->writeUint16( clusterSc );  
   writer->writeUint16( clusterMin );  
   writer->writeUint16( clusterMax );  
   writer->writeFloat( (float)clusterId ); //Uint32
   writer->writeUint32( clusterGeo_state );
   writer->writeUint8( clusterPostdata_state );
   writer->writeBool( clusterPre_state );
   writer->writeUint8( clusterTimesteps );
   writer->writeUint8( clusterTeacher_state ); 
   writer->writeUint16( currentFrame );
   writer->writeFloat( clusterTime_since_start );
   writer->writeUint32( clusterFrameNumber );
   return vpr::ReturnStatus::Succeed;
}

template<>
inline vpr::ReturnStatus vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::readObject(vpr::ObjectReader* reader)
{
   clusterIso_value        = (int)reader->readFloat();  
   clusterSc               = reader->readUint16();  
   clusterMin              = reader->readUint16();  
   clusterMax              = reader->readUint16();  
   clusterId               = (int)reader->readFloat();
   clusterGeo_state        = reader->readUint32();
   clusterPostdata_state   = reader->readUint8();
   clusterPre_state        = reader->readBool();
   clusterTimesteps        = reader->readUint8();
   clusterTeacher_state    = reader->readUint8(); 
   currentFrame            = reader->readUint16(); 
   clusterTime_since_start = reader->readFloat();
   clusterFrameNumber      = reader->readUint32();
   return vpr::ReturnStatus::Succeed;
}
}
