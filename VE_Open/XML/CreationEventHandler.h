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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CREATION_EVENT_HANDLER_H
#define CREATION_EVENT_HANDLER_H
/*!\file CreationEventHandler.h
  CreationEventHandler API
  */
/*!\class CreationEventHandler
 * Create XMLObject event handling.
 */

namespace VE_XML
{
   class XMLObject;
   class Transform;
   class FloatArray;

   class OneDDoubleArray;
   class OneDIntArray;
   class OneDStringArray;

   class TwoDDoubleArray;
   class TwoDIntArray;
   class TwoDStringArray;

   class ThreeDDoubleArray;
   class ThreeDIntArray;
   class ThreeDStringArray;
}
namespace VE_CAD
{
   class CADAssembly;
   class CADPart;
   class CADClone;
   class CADAttribute;
}



template<class T> 
class CreationEventHandler{
public:
   ///Constructor
   CreationEventHandler(){}

   ///Destructor
   virtual ~CreationEventHandler(){}

   ///Create a new XMLObject
   T* GetNewXMLObject();
protected:
};
#endif// VE_CREATION_EVENT_HANDLER_H
typedef CreationEventHandler<VE_CAD::CADAssembly> AssemblyCreator;
typedef CreationEventHandler<VE_CAD::CADPart> PartCreator;
typedef CreationEventHandler<VE_CAD::CADClone> CloneCreator;
typedef CreationEventHandler<VE_CAD::CADAttribute> AttributeCreator;

typedef CreationEventHandler<VE_XML::Transform> TransformCreator;
typedef CreationEventHandler<VE_XML::FloatArray> FloatArrayCreator;

typedef CreationEventHandler<VE_XML::OneDDoubleArray> OneDDoubleArrayCreator;
typedef CreationEventHandler<VE_XML::OneDIntArray> OneDIntArrayCreator;
typedef CreationEventHandler<VE_XML::OneDStringArray> OneDStringArrayCreator;

typedef CreationEventHandler<VE_XML::TwoDDoubleArray> TwoDDoubleArrayreator;
typedef CreationEventHandler<VE_XML::TwoDIntArray> TwoDIntArrayCreator;

typedef CreationEventHandler<VE_XML::ThreeDDoubleArray> ThreeDDoubleArrayCreator;
typedef CreationEventHandler<VE_XML::ThreeDIntArray> ThreeDIntArrayCreator;


