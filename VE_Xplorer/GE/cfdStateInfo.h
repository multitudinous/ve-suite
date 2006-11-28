/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <vpr/IO/SerializableObject.h>
#include <vpr/IO/BufferObjectReader.h>
#include <vpr/IO/BufferObjectWriter.h>
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
   double   clusterIso_value;
   double   clusterSc;
   double   clusterMin;
   double   clusterMax;
   double   clusterId;
   double   clusterGeo_state;
   double   clusterPostdata_state;
   bool     clusterPre_state;
   double   clusterTimesteps;
   double   clusterTeacher_state; 
   short    clusterClientInfoFlag; 
   int      currentFrame; // the index of the current frame
   float    clusterTime_since_start;
   long     clusterFrameNumber;
   float    clusterQuatCamIncrement;
   std::string clusterXMLCommands;
};
}

namespace vpr
{
template<>
inline vpr::ReturnStatus vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::writeObject(vpr::ObjectWriter* writer)
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
   //writer->writeString( clusterXMLCommands );
   vpr::BufferObjectWriter* bufwriter =
      static_cast< vpr::BufferObjectWriter* >( writer );
 
   bufwriter->writeUint64(clusterXMLCommands.size());
 
   for(unsigned i = 0; i < clusterXMLCommands.length(); ++i)
   {
     bufwriter->writeRaw((vpr::Uint8*)&(clusterXMLCommands[i]),1);
   }
   return vpr::ReturnStatus::Succeed;
}

template<>
inline vpr::ReturnStatus vpr::SerializableObjectMixin< ClusterVariables::StateVariables >::readObject(vpr::ObjectReader* reader)
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
   //clusterXMLCommands      = reader->readString();
   vpr::BufferObjectReader* bufreader =
      static_cast< vpr::BufferObjectReader* >( reader );
 
   vpr::Uint64 str_len = bufreader->readUint64();
 
   if ( !clusterXMLCommands.empty() ) 
      clusterXMLCommands.clear();
 
   for(unsigned i = 0; i < str_len; ++i)
   {
     clusterXMLCommands += (char)(*bufreader->readRaw(1));
   }
   return vpr::ReturnStatus::Succeed;
}
}
