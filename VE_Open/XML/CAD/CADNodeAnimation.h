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
 * Date modified: $Date: 2006-07-08 22:04:36 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4905 $
 * Author:        $Author: mccdo $
 * Id:            $Id: CADNodeAnimation.h 4905 2006-07-09 03:04:36Z mccdo $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_NODE_ANIMATION_H
#define CAD_NODE_ANIMATION_H

#include "VE_Open/XML/XMLObject.h"
#include <xercesc/dom/DOM.hpp>
#include <string>

/*!\file CADNodeAnimation.h
  CADNodeAnimation API
  */
/*!\class VE_CAD::CADNodeAnimation
 * This class holds data for describing animations for a CADNode.
 */


namespace VE_CAD
{

class VE_CAD_EXPORTS CADNodeAnimation: public VE_XML::XMLObject
{
public:
   ///Constructor
   CADNodeAnimation();
   virtual ~CADNodeAnimation();

   ///Set the source of animation file .
   ///\param attributeType The type of attribute.
   void SetAnimationFileName(std::string fileName);

   ///Set the source of animation file based on file extension.
   ///Currently valid sources are "osg" "txt".
   ///\param fileSourceType The type of attribute.
   void SetFileType(std::string fileSourceType);

   ///Set the play mode.
   ///Valid types are "Loop" or "Once".
   ///\param playMode The play mode for the animation.
   void SetPlayMode(std::string playMode);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlNode);


   ///Get the file type. 
   std::string GetFileType();

   ///The play mode of the animation.
   std::string GetPlayMode();

   ///Get the name of animation file.
   std::string GetAnimationFileName();

   ///Copy constructor
   CADNodeAnimation(const CADNodeAnimation& rhs);

   ///Equal operator
   CADNodeAnimation& operator=(const CADNodeAnimation& rhs);

protected:
   
   ///Internally update the XML data for this element.
   ///\param input The XML element information
   virtual void _updateVEElement(std::string input);

   std::string _fileSourceType;///<The type of attribute
   std::string _animationFileName;///<The actual name of the file.
   std::string _playMode;///<The play mode of the animation.
};
}
#endif// CAD_NODE_ANIMATION_H
