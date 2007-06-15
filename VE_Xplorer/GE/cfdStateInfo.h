/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_STATE_INFO_H
#define CFD_STATE_INFO_H

#include <vpr/IO/SerializableObject.h>
#include <vpr/IO/BufferObjectReader.h>
#include <vpr/IO/BufferObjectWriter.h>

#include <vrj/vrjParam.h>

#include <string>
/*!\file cfdStateInfo.h
StateVariables API
*/

/*!\class ClusterVariables::StateVariables
*
*/

namespace ClusterVariables
{
struct StateVariables
{
   double      clusterIso_value;
   double      clusterSc;
   double      clusterMin;
   double      clusterMax;
   double      clusterId;
   double      clusterGeo_state;
   double      clusterPostdata_state;
   bool        clusterPre_state;
   double      clusterTimesteps;
   double      clusterTeacher_state; 
   short       clusterClientInfoFlag; 
   int         currentFrame; // the index of the current frame
   float       clusterTime_since_start;
   long        clusterFrameNumber;
   float       clusterQuatCamIncrement;
   float       clusterMatrix[16];
   std::string clusterXMLCommands;
};
}

namespace vpr
{
template<>
#if __VJ_version <= 2000003
   inline vpr::ReturnStatus vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::writeObject(vpr::ObjectWriter* writer)
#elif __VJ_version > 2000003
   inline void vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::writeObject(vpr::ObjectWriter* writer)
#endif
{ 
   writer->writeDouble( clusterIso_value );  
   writer->writeDouble( clusterSc );  
   writer->writeDouble( clusterMin );  
   writer->writeDouble( clusterMax );  
   writer->writeDouble( clusterId ); 
   writer->writeDouble( clusterGeo_state );
   writer->writeDouble( clusterPostdata_state );
   writer->writeBool( clusterPre_state );
   writer->writeDouble( clusterTimesteps );
   writer->writeDouble( clusterTeacher_state ); 
   writer->writeUint16( currentFrame );
   writer->writeFloat( clusterTime_since_start );
   writer->writeUint32( clusterFrameNumber );
   writer->writeFloat( clusterQuatCamIncrement );

   for(int i=0;i<16;i++){
      writer->writeFloat(clusterMatrix[i]);
   }

   //writer->writeString( clusterXMLCommands );
 
   writer->writeUint64(clusterXMLCommands.length());
 
   vpr::BufferObjectWriter* bufwriter =
      static_cast< vpr::BufferObjectWriter* >( writer );
   for(unsigned i = 0; i < clusterXMLCommands.length(); ++i)
   {
     bufwriter->writeRaw((vpr::Uint8*) &(clusterXMLCommands[i]),1);
   }
#if __VJ_version <= 2000003
   return vpr::ReturnStatus::Succeed;
#elif __VJ_version > 2000003
#endif
}

template<>
#if __VJ_version <= 2000003
inline vpr::ReturnStatus vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::readObject(vpr::ObjectReader* reader)
#elif __VJ_version > 2000003
inline void vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::readObject(vpr::ObjectReader* reader)
#endif
{
   clusterIso_value        = reader->readDouble();  
   clusterSc               = reader->readDouble();  
   clusterMin              = reader->readDouble();  
   clusterMax              = reader->readDouble();  
   clusterId               = reader->readDouble();
   clusterGeo_state        = reader->readDouble();
   clusterPostdata_state   = reader->readDouble();
   clusterPre_state        = reader->readBool();
   clusterTimesteps        = reader->readDouble();
   clusterTeacher_state    = reader->readDouble(); 
   currentFrame            = reader->readUint16(); 
   clusterTime_since_start = reader->readFloat();
   clusterFrameNumber      = reader->readUint32();
   clusterQuatCamIncrement = reader->readFloat();

   for(int i=0;i<16;i++){
      clusterMatrix[i]=reader->readFloat();
   }

   //clusterXMLCommands      = reader->readString(); 
   vpr::Uint64 str_len = reader->readUint64();
 
   vpr::BufferObjectReader* bufreader =
      static_cast< vpr::BufferObjectReader* >( reader );
   clusterXMLCommands.clear();
   char tempChar;
   for(unsigned i = 0; i < str_len; ++i)
   {
     tempChar = (char)(*bufreader->readRaw(1));
     clusterXMLCommands += tempChar;
   }
#if __VJ_version <= 2000003
   return vpr::ReturnStatus::Succeed;
#elif __VJ_version > 2000003
#endif
}
}
#endif// CFD_STATE_INFO_H
