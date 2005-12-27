#ifndef _CAD_PART_H_
#define _CAD_PART_H_
/*!\file CADPart.h
 * CADPart API
 */

/*!\class VE_CAD::CADPart 
 * Class to represent a part file (the actual CAD geometry)
 */
#include "VE_Installer/include/VEConfig.h"
#include <VE_CAD/CADNode.h>
#include <string>

namespace VE_CAD{
class VE_CAD_EXPORTS CADPart: public VE_CAD::CADNode{
public:
   ///\param name The name of the part
   ///Constructor
   CADPart(std::string name=std::string("Part"));

   ///Destructor
   virtual ~CADPart();

   ///\param cadFileName The name of the part
   ///Set the name of the CAD file this node represents
   void SetCADFileName(std::string cadFileName);

   ///Get the name of the CAD file this node represents
   std::string GetCADFileName();

protected:
   std::string _cadFileName; ///<The name of the CAD file on disk
};
}
#endif// _CAD_PART_H_
