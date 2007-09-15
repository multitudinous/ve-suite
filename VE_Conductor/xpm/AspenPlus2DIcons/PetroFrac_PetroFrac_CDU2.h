#ifndef GETVESUITE_PetroFrac_PetroFrac_CDU2_H
#define GETVESUITE_PetroFrac_PetroFrac_CDU2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_PetroFrac_PetroFrac_CDU2( void )
{
    unsigned char osgData[ 8969 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,136,0,139,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,40,175,152,63,105,223,218,118,15,217,174,15,130,246,150,127,5,254,48,124,125,241,191,199,223,140,19,252,20,248,109,240,219,224,164,255,0,6,236,124,85,169,248,170,199,224,223,198,15,143,26,181,253,254,173,241,227,227,7,129,252,59,164,248,126,211,225,223,192,239,29,79,44,179,235,171,115,37,204,86,150,182,182,151,18,220,0,158,62,63,108,159,218,40,255,0,206,39,127,111,255,0,252,56,223,240,75,47,254,153,101,0,125,255,0,95,51,254,215,31,181,199,193,31,216,147,224,143,137,62,61,124,122,241,36,154,47,133,52,89,33,210,116,61,15,73,134,223,81,241,183,196,111,27,106,54,247,147,248,123,225,207,195,159,15,79,121,7,252,36,126,53,212,126,193,122,208,194,211,91,218,89,218,105,247,186,190,175,123,166,232,122,110,167,169,217,126,124,254,215,159,240,80,31,218,211,225,79,193,171,223,139,190,16,255,0,130,115,126,219,254,2,213,254,24,120,131,65,241,36,94,29,241,151,138,191,224,152,122,255,0,130,254,59,79,170,79,47,130,116,127,217,207,91,180,240,119,237,237,226,15,25,91,248,131,198,126,36,241,102,137,165,120,58,127,0,105,90,183,140,162,241,252,254,19,58,119,133,126,34,218,29,67,225,143,141,63,153,63,248,41,127,237,85,162,255,0,193,66,191,224,164,58,253,223,195,175,28,248,115,226,215,236,217,240,43,78,209,62,21,252,9,241,55,129,181,205,90,251,192,154,234,75,164,105,154,239,197,255,0,28,104,203,172,107,23,26,94,181,171,222,252,73,187,214,180,191,248,73,52,43,75,109,59,196,90,7,195,79,11,75,109,54,175,97,107,101,169,94,227,94,178,161,77,205,171,216,105,93,164,123,223,199,223,248,43,55,252,20,87,246,234,144,248,127,225,93,213,199,236,105,240,138,235,236,77,39,134,190,12,248,143,83,212,62,45,234,210,193,255,0,8,62,190,163,196,255,0,31,215,77,210,245,75,8,160,215,188,55,175,53,178,248,78,203,194,175,54,147,226,107,173,39,94,131,95,180,101,153,188,7,196,63,178,223,199,111,218,18,61,58,95,218,3,227,63,198,143,142,223,240,138,139,151,240,135,252,46,31,137,190,62,248,164,60,45,31,136,22,212,235,109,225,211,226,173,106,228,232,38,255,0,251,35,70,251,96,133,98,251,79,246,61,169,144,201,246,116,9,247,191,236,215,240,119,71,91,61,58,218,13,57,45,173,130,195,149,205,188,146,57,220,101,220,210,180,220,150,59,93,137,5,153,185,103,57,34,191,102,126,22,254,206,26,70,167,107,105,43,218,110,42,172,86,22,13,228,27,99,11,44,9,34,40,84,33,75,18,163,113,10,10,128,78,9,31,31,139,206,43,115,53,207,100,254,229,177,209,26,125,150,135,242,227,165,252,3,253,166,255,0,103,237,22,243,195,95,1,127,104,127,218,15,224,167,133,47,102,184,241,38,161,225,47,132,223,24,190,38,124,53,240,214,161,226,123,187,123,125,50,93,126,247,195,254,22,215,237,226,191,215,39,176,210,52,139,89,174,154,47,57,237,244,139,104,76,140,144,34,199,245,183,236,219,255,0,5,182,253,181,191,99,63,236,143,1,254,214,126,28,187,253,173,254,15,232,255,0,217,90,72,241,124,243,218,120,107,246,143,240,159,135,180,181,240,47,135,110,46,163,241,59,66,116,207,140,239,99,160,105,30,45,189,22,254,33,16,120,139,95,215,117,241,54,183,227,251,56,54,162,254,217,124,94,253,158,52,141,42,210,240,139,87,42,94,225,166,141,179,229,164,108,197,237,213,18,85,216,33,89,24,149,80,227,4,163,3,149,34,191,18,127,105,239,131,122,53,205,141,237,181,206,155,231,108,18,121,82,163,90,69,36,69,85,145,10,229,152,23,80,219,125,25,9,87,12,165,183,86,11,54,173,205,20,230,228,188,245,93,5,42,105,90,232,254,194,254,12,124,103,248,91,251,67,252,45,240,87,198,175,130,190,53,209,254,33,252,47,248,135,163,38,185,225,47,22,232,111,112,45,53,11,79,62,123,43,203,91,187,43,232,33,187,209,53,235,29,86,210,254,195,84,210,239,224,181,212,180,157,75,76,187,211,53,59,75,77,66,210,230,218,47,78,175,226,111,254,13,243,253,165,47,126,4,126,220,191,17,255,0,99,79,19,107,150,186,31,195,239,218,111,194,254,35,241,119,195,207,12,95,77,226,125,80,221,126,209,31,10,172,79,136,46,236,60,29,101,165,106,114,232,254,23,151,84,248,25,166,252,71,188,215,239,239,44,35,155,85,139,224,207,134,236,255,0,181,150,91,27,59,29,67,251,100,175,175,163,85,86,167,26,137,91,152,194,74,205,160,162,138,43,81,5,20,81,64,5,20,81,64,5,20,81,64,31,206,95,199,255,0,218,123,227,39,199,189,87,225,175,237,43,224,45,67,195,186,103,132,62,3,254,208,250,255,0,136,191,97,207,135,30,25,240,171,248,159,226,15,237,39,241,79,86,248,81,241,107,225,62,139,107,170,105,190,39,213,108,37,213,147,196,31,3,188,93,241,115,81,158,197,135,130,236,252,15,224,255,0,23,120,155,196,254,48,214,116,203,47,1,221,120,243,195,159,125,255,0,193,41,126,36,254,210,255,0,21,254,2,252,81,241,151,237,105,241,167,195,63,21,190,50,95,254,210,191,21,127,180,60,23,224,175,135,26,87,129,124,27,251,50,248,106,91,79,10,223,120,67,246,110,240,151,136,180,219,43,121,190,51,120,103,72,208,47,108,181,171,15,24,106,177,13,114,254,215,226,20,90,118,181,255,0,19,45,38,224,15,206,107,191,216,183,246,139,253,152,190,54,124,26,212,226,248,161,240,173,252,109,251,122,252,107,248,143,240,35,195,62,30,209,188,61,175,207,224,31,216,71,195,58,247,194,255,0,139,223,181,238,187,240,115,224,20,243,165,153,241,255,0,194,219,79,13,126,207,154,229,189,222,173,30,141,240,255,0,85,241,167,136,62,28,120,15,251,87,75,209,60,59,101,225,237,43,225,167,238,175,236,207,240,7,66,253,155,62,19,105,31,13,244,173,82,79,19,106,205,168,235,30,40,241,199,142,47,52,203,61,47,85,241,215,142,60,73,122,247,250,223,136,53,24,109,158,89,126,205,20,127,98,211,52,168,111,46,245,27,205,63,65,240,246,147,164,205,169,95,141,61,46,100,243,176,209,199,71,19,91,219,203,154,139,213,126,138,41,108,146,222,250,182,91,229,229,141,190,46,167,123,227,255,0,133,190,4,248,164,124,21,31,143,244,35,226,91,47,135,255,0,16,60,55,241,75,195,122,53,230,167,172,69,225,198,241,223,131,62,219,115,224,173,119,196,158,26,178,212,34,211,252,109,253,137,226,11,155,45,123,69,181,214,237,181,11,61,35,197,94,23,208,60,91,166,219,218,248,159,195,122,6,173,166,255,0,152,175,252,18,239,254,73,255,0,195,191,251,16,252,55,233,255,0,64,205,36,127,42,255,0,82,170,255,0,45,95,248,37,215,252,147,255,0,135,127,246,33,248,111,255,0,77,154,77,44,211,253,219,230,16,221,250,31,214,79,236,229,254,170,195,232,159,204,87,238,95,194,127,249,7,191,253,122,207,255,0,162,36,175,195,79,217,203,253,85,135,209,63,154,215,238,95,194,127,249,7,191,253,122,207,255,0,162,36,175,207,241,127,20,190,95,161,213,13,159,169,243,79,237,11,255,0,30,119,127,245,216,255,0,232,185,171,240,23,246,157,255,0,85,127,254,236,159,250,12,149,251,245,251,66,255,0,199,157,231,253,118,63,250,46,106,252,5,253,167,127,213,95,255,0,187,39,254,131,37,109,129,248,163,242,252,208,170,116,63,37,63,224,148,127,242,158,15,216,7,254,198,95,218,103,255,0,88,171,246,149,175,244,145,175,243,110,255,0,130,81,255,0,202,120,63,96,31,251,25,127,105,159,253,98,175,218,86,191,210,70,191,67,193,255,0,187,211,244,57,103,241,48,162,138,43,164,144,162,138,40,0,162,138,40,0,162,138,40,3,192,63,104,79,217,139,225,23,237,65,163,248,15,70,248,181,105,241,3,254,45,127,196,5,248,165,240,247,196,31,11,126,53,252,108,253,159,252,119,224,255,0,29,255,0,194,9,227,191,134,51,107,186,23,196,127,128,63,16,188,51,226,11,47,63,225,255,0,196,223,30,105,55,86,171,169,253,142,238,207,196,215,17,220,219,203,251,178,159,63,255,0,195,180,255,0,103,95,250,40,223,183,255,0,254,45,139,254,10,155,255,0,209,145,95,64,126,208,159,180,239,194,47,217,127,71,240,30,179,241,110,239,226,7,252,93,15,136,11,240,183,225,239,135,254,22,252,19,248,217,251,64,120,239,198,30,59,255,0,132,19,199,127,19,166,208,180,47,135,31,0,126,30,248,159,196,23,190,71,195,255,0,134,94,60,213,174,174,151,76,251,29,165,159,134,110,36,185,184,139,247,97,254,127,255,0,135,150,126,206,191,244,78,127,111,255,0,252,84,239,252,21,55,255,0,160,222,128,15,248,118,159,236,235,255,0,69,27,246,255,0,255,0,197,177,127,193,83,127,250,50,43,252,245,63,224,151,127,242,79,254,29,255,0,216,135,225,190,216,255,0,152,102,147,95,221,103,237,47,255,0,5,113,248,97,240,111,225,102,173,241,147,193,255,0,12,191,107,253,79,195,255,0,9,62,223,241,23,227,23,134,252,127,255,0,4,191,255,0,130,154,248,7,254,18,79,130,126,23,240,230,189,127,241,6,15,5,124,85,241,135,236,203,163,248,99,225,111,196,13,54,201,109,117,253,55,81,241,140,201,225,13,75,254,17,9,188,47,226,29,95,193,26,127,136,37,248,141,224,223,225,79,254,9,119,255,0,36,255,0,225,217,206,127,226,131,240,223,167,253,3,52,159,74,243,115,79,247,111,153,112,221,250,31,214,79,236,229,254,170,195,232,159,205,107,247,47,225,63,252,131,223,254,189,103,255,0,209,18,87,225,167,236,229,254,170,195,232,159,205,107,247,47,225,63,252,131,223,254,189,103,255,0,209,18,87,231,248,191,138,95,47,208,234,134,207,212,249,167,246,133,255,0,143,59,207,250,236,127,244,92,213,248,11,251,78,255,0,170,191,255,0,118,79,253,6,74,253,250,253,161,127,227,206,243,254,187,31,253,23,53,126,2,254,211,191,234,175,255,0,221,147,255,0,65,146,182,192,252,113,249,126,104,85,58,31,146,159,240,74,63,249,79,7,236,3,255,0,99,47,237,51,255,0,172,85,251,74,215,250,72,215,249,183,127,193,40,255,0,229,60,31,176,15,253,140,191,180,207,254,177,87,237,43,95,233,35,95,161,225,63,221,233,250,28,179,248,152,81,69,21,210,72,81,69,20,0,81,69,20,0,81,69,20,1,252,255,0,254,213,95,182,39,139,126,34,248,195,225,167,237,15,240,243,193,62,24,135,224,103,236,29,251,71,120,135,226,103,134,124,85,174,235,154,174,191,168,126,210,254,43,215,126,21,252,91,253,143,244,255,0,11,124,62,211,124,19,166,93,63,217,124,69,31,237,7,226,219,31,2,166,128,158,49,214,60,93,175,120,199,192,147,105,186,83,221,205,55,132,53,175,188,63,224,154,31,180,111,237,41,251,84,124,13,241,239,197,207,218,115,194,31,5,190,26,120,143,80,248,251,241,63,70,248,113,240,151,225,70,177,226,29,111,197,223,9,190,13,232,205,161,193,224,111,2,254,210,55,154,206,167,119,105,55,237,19,11,73,171,222,106,183,30,31,146,63,14,106,90,54,181,225,253,95,72,130,24,53,2,131,242,35,64,253,159,127,107,143,217,207,226,151,193,181,241,63,195,159,5,13,47,246,131,248,199,241,27,225,159,236,1,240,95,80,248,135,103,171,219,126,199,114,235,223,10,254,47,124,78,91,111,142,119,86,235,123,99,172,126,208,182,223,179,71,131,62,35,105,218,223,140,116,61,87,199,246,154,63,135,188,25,226,143,2,248,43,87,190,255,0,132,195,94,212,254,47,126,240,126,199,159,179,189,207,236,217,240,121,60,39,175,234,90,102,187,241,23,197,254,40,215,190,36,124,85,241,14,136,117,21,209,181,111,28,248,147,236,118,98,223,72,139,80,40,63,179,52,159,9,232,222,21,208,109,238,98,178,210,198,167,23,133,19,87,184,210,236,111,245,11,200,71,157,134,150,53,226,43,71,17,31,221,110,158,203,201,71,190,155,183,215,69,181,221,53,27,43,110,122,63,198,79,130,190,21,248,233,167,120,7,64,241,198,161,226,1,225,111,3,124,96,248,95,241,174,227,195,26,53,214,157,167,233,222,54,241,87,193,95,21,89,124,74,248,83,167,248,178,254,77,38,93,74,15,15,232,191,25,60,59,240,243,198,17,69,162,223,232,247,55,250,175,195,93,51,77,213,238,181,15,10,94,248,139,195,154,239,249,154,255,0,193,46,191,228,159,252,59,255,0,177,15,195,159,250,109,210,133,127,169,85,127,150,175,252,18,235,254,73,255,0,195,191,251,16,252,55,255,0,166,205,38,150,105,254,237,243,28,55,126,135,245,147,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,248,105,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,249,254,47,226,151,203,244,58,161,179,245,62,105,253,161,127,227,206,243,254,187,31,253,23,53,126,2,254,211,191,234,175,255,0,221,147,255,0,65,146,191,126,191,104,95,248,243,188,255,0,174,199,255,0,69,205,95,128,191,180,239,250,171,255,0,247,100,255,0,208,100,173,176,63,28,126,95,154,21,78,135,226,175,252,19,211,226,95,135,62,15,255,0,193,105,127,97,239,136,222,44,211,126,32,106,254,31,240,239,137,127,104,95,237,13,63,225,111,194,127,138,127,28,124,119,112,53,111,217,15,246,133,208,173,127,176,190,22,252,20,240,119,136,60,79,226,141,151,218,165,179,221,127,102,104,247,127,98,179,142,227,81,189,251,62,159,103,117,115,15,247,163,255,0,15,44,253,157,127,232,156,254,223,255,0,248,169,223,248,42,111,255,0,65,189,127,5,223,240,79,79,139,31,11,62,7,127,193,105,127,97,255,0,138,95,26,254,37,248,3,224,255,0,195,47,11,248,151,246,133,255,0,132,155,226,55,197,47,25,120,119,225,255,0,129,60,59,253,183,251,33,254,208,190,29,209,191,183,124,93,226,205,70,211,79,210,62,215,226,13,91,74,177,181,251,69,196,127,104,188,213,45,237,98,223,60,209,198,223,222,143,252,61,139,254,9,101,255,0,73,44,253,128,63,241,50,63,103,95,254,120,245,250,30,19,248,20,206,89,252,76,63,225,229,159,179,175,253,19,159,219,255,0,255,0,21,59,255,0,5,77,255,0,232,55,163,254,30,89,251,58,255,0,209,57,253,191,255,0,241,83,191,240,84,223,254,131,122,63,225,236,95,240,75,47,250,73,103,236,1,255,0,137,145,251,58,255,0,243,198,163,254,30,197,255,0,4,178,255,0,164,150,126,192,31,248,153,31,179,175,255,0,60,106,233,36,63,225,229,159,179,175,253,19,159,219,255,0,255,0,21,59,255,0,5,77,255,0,232,55,163,254,30,89,251,58,255,0,209,57,253,191,255,0,241,83,191,240,84,223,254,131,122,63,225,236,95,240,75,47,250,73,103,236,1,255,0,137,145,251,58,255,0,243,198,163,254,30,197,255,0,4,178,255,0,164,150,126,192,31,248,153,31,179,175,255,0,60,106,0,63,225,229,159,179,175,253,19,159,219,255,0,255,0,21,59,255,0,5,77,255,0,232,55,174,127,197,159,240,85,143,217,47,192,94,21,241,55,142,188,117,225,239,219,127,193,126,8,240,95,135,245,159,22,120,199,198,62,44,255,0,130,91,127,193,79,60,57,225,95,9,248,87,195,154,117,206,177,226,31,19,120,151,196,58,199,236,127,13,166,133,225,251,13,34,206,242,234,246,246,234,104,173,173,109,173,100,158,121,35,137,25,135,195,95,182,127,252,23,51,225,7,130,126,36,254,206,95,8,63,97,79,140,95,178,79,237,73,226,111,139,7,226,143,137,190,36,120,163,193,159,22,188,47,241,211,66,248,95,225,15,134,75,240,255,0,79,179,208,245,175,15,124,29,248,143,111,113,162,120,147,196,154,191,196,168,102,210,117,27,253,65,109,82,219,225,214,179,2,105,186,132,179,139,141,55,215,190,12,104,95,20,255,0,107,175,129,127,16,126,10,254,210,159,31,124,113,241,63,225,231,198,15,134,222,45,248,105,241,31,78,255,0,132,71,224,255,0,130,245,63,19,248,31,226,23,133,245,79,11,120,203,64,147,88,240,23,195,173,53,244,136,239,52,13,83,80,128,92,233,201,101,123,108,110,76,182,215,81,204,145,200,158,118,55,51,195,224,90,85,148,155,223,69,125,11,140,37,53,120,159,180,212,87,225,159,237,125,251,78,254,209,63,3,117,27,45,95,193,191,31,252,92,214,62,22,213,180,237,94,251,195,186,207,195,79,130,186,142,153,227,11,109,54,250,11,219,191,12,248,154,234,31,6,65,123,6,129,127,107,11,90,221,62,145,117,164,106,113,193,121,43,217,234,86,183,34,27,136,191,7,245,239,248,60,254,219,224,222,183,172,124,51,248,175,251,15,207,241,23,199,190,19,212,239,172,181,175,25,252,61,248,173,31,195,15,8,107,80,221,93,75,169,104,143,165,120,27,196,158,28,241,93,238,137,36,30,31,189,210,173,174,188,237,126,248,93,94,89,220,94,68,45,33,184,142,198,218,240,121,134,31,26,155,163,205,167,117,110,221,175,220,82,139,139,179,63,176,223,218,119,246,98,131,246,148,131,224,189,221,167,198,143,140,31,0,188,109,240,7,227,4,255,0,26,254,27,124,73,248,41,7,193,171,255,0,21,105,190,42,190,248,55,241,131,224,62,173,97,168,105,63,30,62,15,248,227,195,186,183,135,238,254,30,124,113,241,212,18,197,62,132,215,49,220,201,105,117,107,119,111,45,184,47,227,255,0,240,198,255,0,180,87,253,37,139,246,255,0,255,0,195,115,255,0,4,178,255,0,233,105,215,223,244,87,113,39,192,31,240,198,255,0,180,87,253,37,139,246,255,0,255,0,195,115,255,0,4,178,255,0,233,105,215,249,234,127,193,46,255,0,228,64,248,119,211,254,68,63,13,244,255,0,176,94,147,95,234,85,95,229,171,255,0,4,186,255,0,146,127,240,239,254,196,63,13,255,0,233,179,73,175,55,52,255,0,118,249,151,13,223,161,253,100,254,206,95,234,172,62,137,252,214,191,114,254,19,255,0,200,61,255,0,235,214,127,253,17,37,126,26,126,206,95,234,172,62,137,252,214,191,114,254,19,255,0,200,61,255,0,235,214,127,253,17,37,126,127,139,248,165,242,253,14,168,108,253,79,154,127,104,95,248,243,188,255,0,174,199,255,0,69,205,95,128,191,180,239,250,171,255,0,247,100,255,0,208,100,175,223,175,218,23,254,60,239,63,235,177,255,0,209,115,87,224,47,237,59,254,170,255,0,253,217,63,244,25,43,108,15,199,31,151,230,133,83,161,249,41,255,0,4,163,255,0,148,240,126,192,63,246,50,254,211,31,250,197,95,180,173,127,164,141,127,155,119,252,18,143,254,83,193,251,0,255,0,216,203,251,76,255,0,235,21,126,210,181,254,146,53,250,30,19,253,222,159,161,203,63,137,133,20,81,93,36,133,20,81,64,31,201,191,252,28,109,225,205,99,197,31,181,151,252,18,203,75,209,60,123,226,207,135,23,199,192,95,183,196,227,196,126,12,179,240,53,246,178,177,38,183,251,15,163,89,136,62,33,120,47,94,211,254,204,237,42,59,19,96,102,13,110,129,37,68,50,43,253,31,251,21,252,59,255,0,130,128,197,161,193,103,224,47,218,195,246,85,131,70,179,178,142,215,76,188,248,171,251,13,252,76,241,239,142,47,116,235,120,100,138,206,235,198,26,231,195,191,219,191,192,250,54,173,226,169,45,145,31,80,186,210,124,55,160,233,147,221,201,44,182,26,38,151,106,209,88,193,243,135,252,28,109,225,205,99,197,31,181,151,252,18,203,75,209,60,123,226,207,135,23,199,192,95,183,196,227,196,126,12,179,240,53,246,178,177,38,183,251,15,163,89,136,62,33,120,47,94,211,254,204,237,42,59,19,96,102,13,110,129,37,68,50,43,253,33,251,21,124,59,255,0,130,129,69,161,193,103,224,47,218,195,246,85,183,209,172,236,163,181,211,47,62,42,254,195,127,19,60,123,227,139,221,54,222,25,34,178,186,241,134,185,240,239,246,240,240,62,141,171,120,170,75,84,141,245,11,173,39,195,122,6,153,61,220,146,203,97,162,105,118,173,21,140,31,45,159,59,77,46,122,81,186,90,84,141,250,247,229,127,113,189,29,165,191,245,99,227,143,248,40,7,195,207,219,78,242,215,93,176,248,161,251,81,124,12,125,26,101,144,223,234,63,1,255,0,100,143,19,124,46,241,228,77,12,94,125,176,208,117,207,140,63,181,63,196,253,15,79,142,75,184,224,138,240,95,120,103,83,105,108,102,186,134,209,244,251,217,45,181,43,63,243,137,253,169,236,174,116,207,218,3,226,101,133,246,179,169,120,134,238,223,89,180,73,181,173,98,13,30,29,78,253,155,69,211,28,75,119,30,129,165,216,217,171,133,96,131,200,180,129,118,198,164,169,125,204,223,232,235,255,0,5,0,248,121,251,105,222,90,235,182,31,20,63,106,47,129,143,163,204,178,27,253,71,224,63,236,145,226,111,133,222,60,137,161,143,207,182,26,14,185,241,135,246,167,248,159,161,233,241,201,119,20,17,94,11,239,12,234,109,45,140,215,80,218,62,159,123,37,182,165,103,254,113,95,181,61,149,206,153,251,64,124,76,177,190,214,117,47,16,221,219,235,54,137,62,181,172,65,163,195,169,223,187,104,186,99,137,110,227,208,52,187,27,53,144,43,4,30,69,164,11,182,48,74,150,220,205,209,145,191,118,167,189,7,167,216,86,93,53,248,80,170,175,123,111,189,255,0,193,63,221,214,138,40,175,161,49,10,255,0,45,95,248,37,215,252,147,255,0,135,127,246,33,248,111,255,0,77,154,77,127,169,85,127,150,175,252,18,235,254,73,255,0,195,191,251,16,252,55,255,0,166,205,38,188,220,211,253,219,230,92,55,126,135,245,147,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,248,105,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,249,254,47,226,151,203,244,58,161,179,245,62,105,253,161,127,227,206,243,254,187,31,253,23,53,126,2,254,211,191,234,175,255,0,221,147,255,0,65,146,191,126,191,104,95,248,243,188,255,0,174,199,255,0,69,205,95,128,191,180,239,250,171,255,0,247,100,255,0,208,100,173,176,63,28,126,95,154,21,78,135,228,167,252,18,143,254,83,193,251,0,255,0,216,203,251,76,255,0,235,21,126,210,181,254,146,53,254,101,223,240,79,95,134,158,29,248,193,255,0,5,165,253,135,190,29,120,179,82,248,129,164,248,127,196,94,37,253,161,70,161,168,124,46,248,177,241,79,224,119,142,237,255,0,178,63,100,47,218,23,93,180,254,194,248,165,240,83,198,94,31,241,63,133,247,222,233,118,209,221,127,102,106,246,159,109,179,146,227,78,189,243,244,251,187,187,89,255,0,189,47,248,118,159,236,234,127,230,163,126,223,255,0,248,182,31,248,42,111,255,0,70,69,126,135,132,255,0,119,167,232,114,207,226,103,223,244,87,231,63,136,255,0,224,158,223,178,175,131,252,63,174,248,183,197,159,24,255,0,110,111,11,248,87,194,250,54,167,226,47,19,120,151,196,95,240,87,95,248,42,6,137,225,255,0,15,120,127,69,178,159,82,214,117,221,119,89,212,191,108,248,173,244,141,26,207,78,181,184,184,185,186,184,150,56,32,130,222,73,101,116,68,102,31,24,124,122,248,95,251,50,124,61,240,31,194,79,139,95,178,175,237,93,251,75,248,195,199,26,79,237,121,251,4,75,27,216,127,193,86,191,110,79,218,7,194,218,151,194,141,95,246,226,253,158,116,47,143,175,226,111,135,94,54,253,172,252,71,225,239,23,252,60,95,128,186,151,196,183,241,17,213,116,171,221,54,203,66,139,81,212,110,204,16,90,61,212,27,74,164,33,241,77,70,253,218,95,153,39,239,61,21,227,250,119,237,11,240,11,88,241,134,129,240,243,73,248,227,240,127,84,241,255,0,138,197,251,120,95,192,218,119,196,191,5,222,248,195,196,139,165,105,151,218,214,168,116,15,12,219,107,77,123,172,11,109,27,75,212,238,238,62,207,4,158,77,174,157,61,196,155,98,134,71,95,96,170,82,140,149,226,212,151,150,160,124,101,251,92,254,193,223,0,127,109,3,224,61,91,226,189,143,137,180,159,136,31,10,96,241,45,167,194,191,138,94,7,215,34,210,252,101,240,254,199,199,58,199,128,181,95,31,216,104,246,90,238,157,169,104,58,181,166,185,15,195,127,11,217,222,174,177,162,106,102,27,123,86,147,77,54,23,254,85,236,93,143,193,127,217,95,194,31,3,237,82,207,195,222,52,248,133,226,8,18,40,225,81,226,187,159,5,76,219,35,86,92,22,240,247,130,52,238,74,177,7,244,197,125,57,69,97,91,9,134,196,89,215,161,26,182,238,174,82,148,163,179,177,241,47,199,111,216,71,225,127,237,1,21,236,62,41,241,151,196,127,15,173,244,50,67,35,120,72,124,50,133,144,200,28,121,209,159,18,124,50,212,193,152,110,227,120,117,249,6,84,129,131,240,228,191,240,110,103,252,17,211,88,22,183,254,58,253,143,244,79,136,254,48,109,63,76,182,241,23,143,252,85,241,3,226,141,159,137,188,103,170,233,250,117,174,159,115,226,93,126,207,193,126,51,210,52,123,77,98,241,173,124,251,136,180,173,43,76,211,99,150,102,75,29,62,206,217,98,183,143,246,246,138,116,112,184,124,58,181,26,49,166,159,101,96,114,147,221,220,40,162,138,220,144,175,242,213,255,0,130,93,127,201,63,248,119,255,0,98,31,134,255,0,244,217,164,215,250,149,87,249,106,255,0,193,46,191,228,159,252,59,255,0,177,15,195,127,250,108,210,107,205,205,63,221,190,101,195,119,232,127,89,63,179,151,250,171,15,162,127,53,175,220,191,132,255,0,242,15,127,250,245,159,255,0,68,73,95,134,159,179,151,250,171,15,162,127,53,175,220,191,132,255,0,242,15,127,250,245,159,255,0,68,73,95,159,226,254,41,124,191,67,170,27,63,83,230,159,218,23,254,60,239,63,235,177,255,0,209,115,87,224,47,237,59,254,170,255,0,253,217,63,244,25,43,247,235,246,133,255,0,143,59,207,250,236,127,244,92,213,248,11,251,78,255,0,170,191,255,0,118,79,253,6,74,219,3,241,199,229,249,161,84,232,126,42,255,0,193,61,116,223,138,122,191,252,22,147,246,30,211,254,10,248,203,192,31,15,254,38,220,120,151,246,133,255,0,132,103,197,223,20,190,26,120,143,227,7,128,244,159,39,246,67,253,161,46,53,159,237,223,135,62,19,248,177,224,109,67,196,94,127,135,226,213,109,109,126,207,226,157,47,236,151,151,214,247,242,253,182,27,89,52,235,191,239,71,254,21,199,252,21,55,254,143,39,246,0,255,0,197,105,254,209,95,253,54,42,254,27,127,224,148,127,242,158,15,216,7,254,198,95,218,99,255,0,88,171,246,149,175,244,145,175,208,240,159,192,166,114,207,226,103,241,1,255,0,5,68,248,197,241,31,225,55,252,21,51,225,231,129,63,111,143,218,171,246,107,241,6,157,160,254,199,95,11,252,119,240,190,93,19,192,186,151,236,185,240,179,195,147,248,243,227,119,237,7,225,239,28,92,233,62,7,248,201,251,75,124,67,146,235,199,119,208,124,54,240,154,107,90,213,158,187,105,29,238,157,161,232,22,50,233,80,182,146,183,119,191,210,199,236,125,167,105,214,63,15,239,103,179,211,236,45,101,54,18,72,207,13,149,172,101,220,65,18,150,147,108,95,188,36,0,14,236,228,112,120,200,175,231,179,254,11,53,241,99,225,103,194,127,248,45,55,132,181,47,138,95,18,190,31,252,53,211,181,15,248,39,151,236,230,150,55,254,63,241,151,135,60,27,103,124,246,127,180,191,237,158,247,105,103,117,226,61,74,217,46,90,37,184,128,200,17,152,160,153,11,0,24,26,253,69,253,152,63,224,153,255,0,240,78,29,127,192,119,119,154,247,252,19,251,246,37,214,239,23,79,145,214,235,87,253,148,254,4,234,87,42,226,24,200,101,154,243,192,78,202,251,137,231,57,201,175,152,207,213,55,94,211,85,53,75,225,73,174,155,93,175,158,230,180,110,238,180,126,191,35,243,235,254,10,227,241,63,193,191,13,188,45,226,125,99,199,62,43,248,125,224,123,45,94,238,247,67,211,181,47,25,79,224,239,15,217,106,250,252,186,110,165,115,99,162,218,220,107,201,20,90,150,171,37,150,155,125,34,91,41,146,105,33,176,153,132,108,145,200,71,234,223,252,27,217,241,19,196,223,20,191,224,144,191,178,135,140,60,87,226,187,239,26,95,182,161,251,71,120,99,74,215,111,175,198,164,7,130,188,3,251,87,124,114,240,23,195,173,15,76,185,70,41,31,135,52,159,135,254,25,240,206,149,165,65,14,45,237,116,205,22,210,214,217,82,8,99,81,248,61,255,0,5,31,248,49,251,21,254,200,154,23,139,252,85,224,175,132,223,178,239,236,193,47,136,172,245,143,2,91,248,143,194,222,4,248,79,240,86,77,117,181,24,46,53,168,252,27,14,175,164,105,90,105,213,26,118,240,234,221,174,156,36,144,202,116,49,56,133,190,203,189,63,109,191,224,219,155,171,91,239,248,35,95,236,173,123,101,115,5,229,157,231,140,255,0,108,43,171,75,187,89,163,184,182,186,182,184,253,183,255,0,105,9,160,185,183,158,22,41,60,15,19,163,35,169,42,202,193,129,32,215,163,144,198,17,161,46,69,59,105,172,210,95,151,249,145,83,126,151,242,249,31,185,84,81,69,123,230,97,69,20,80,1,69,20,80,1,95,229,171,255,0,4,187,255,0,146,127,240,243,215,254,16,63,14,12,103,167,252,75,52,160,51,207,235,249,241,95,234,85,95,192,191,252,21,111,224,5,247,236,105,255,0,5,73,248,143,226,251,93,54,47,12,124,36,253,170,47,147,246,129,248,113,115,104,190,41,213,116,253,83,197,26,250,89,219,254,208,26,69,207,136,252,93,166,139,103,241,195,124,93,147,196,222,32,188,210,52,237,70,246,215,70,209,254,41,120,108,167,246,101,181,245,158,155,105,195,152,193,207,14,237,209,166,92,26,79,212,253,134,253,156,191,213,105,255,0,238,169,198,125,211,167,76,245,255,0,60,215,238,103,194,127,249,7,191,253,122,205,211,4,127,168,113,215,235,95,204,7,236,217,241,139,71,123,29,54,230,223,82,91,152,91,203,220,219,109,227,104,206,225,24,220,134,44,228,57,216,224,225,144,131,149,235,183,246,123,225,103,237,23,164,105,214,54,185,188,69,216,135,124,178,77,28,104,32,101,127,179,157,133,138,200,173,183,3,98,56,43,177,152,174,226,163,243,252,101,41,221,187,111,111,208,234,131,86,179,122,158,151,251,66,255,0,199,157,231,253,118,63,250,4,222,220,87,224,47,237,59,254,170,255,0,175,43,46,56,231,149,124,117,233,212,117,255,0,26,253,137,248,193,251,65,233,58,149,149,208,55,138,55,52,133,228,89,210,84,104,115,34,192,219,139,50,34,180,170,7,204,21,129,216,161,50,251,135,226,31,237,63,241,147,70,183,176,190,185,185,212,132,59,196,134,40,145,45,37,146,82,234,236,138,187,149,64,145,176,88,130,112,168,55,187,34,134,198,152,24,77,74,55,143,245,161,51,105,218,206,231,230,63,252,18,143,254,83,193,251,0,255,0,216,203,251,76,246,56,255,0,147,42,253,165,187,145,236,107,253,36,107,248,162,255,0,131,124,63,103,29,75,227,135,237,199,241,59,246,199,241,6,151,14,181,240,235,246,110,240,134,189,224,207,1,248,150,225,60,89,163,170,126,208,63,21,173,97,210,238,166,240,189,230,149,166,67,163,120,189,244,159,130,215,159,16,45,124,65,167,222,223,76,250,80,248,187,225,155,245,210,158,91,203,29,66,195,251,93,175,208,112,137,199,15,77,53,203,166,199,52,254,38,127,37,191,240,87,79,3,248,191,195,159,240,85,255,0,4,124,114,241,7,134,181,189,19,224,214,187,251,27,126,207,127,7,244,127,138,154,174,157,115,97,240,251,82,248,171,163,252,116,253,176,188,101,168,252,55,181,241,109,212,107,99,47,142,35,240,150,181,166,234,103,76,19,253,177,172,46,77,218,66,208,199,35,175,217,159,178,151,252,19,155,254,9,153,227,79,2,92,221,94,126,194,127,176,143,138,175,62,193,38,203,171,207,217,139,246,126,214,103,89,69,188,79,191,237,55,30,9,144,171,0,196,146,88,96,124,220,117,175,232,26,138,225,199,229,82,198,205,206,56,185,225,239,109,35,229,111,239,46,197,66,167,46,156,170,71,241,89,251,123,126,203,159,178,199,236,249,101,227,237,83,224,191,192,223,217,155,224,86,179,121,225,253,103,64,190,212,254,27,120,35,224,247,194,203,251,141,26,245,237,239,14,141,123,168,120,106,203,79,118,210,164,191,178,210,230,104,37,115,9,150,202,9,25,55,197,25,95,221,79,248,32,207,194,79,138,31,2,255,0,224,149,255,0,179,175,195,15,140,190,0,241,111,194,255,0,136,186,7,140,63,106,123,253,111,193,30,58,208,181,15,13,120,167,72,179,241,103,237,127,241,239,198,62,26,184,212,180,93,86,8,174,44,146,251,194,218,254,137,168,91,249,145,175,155,107,170,65,50,141,146,41,63,175,244,87,70,7,2,240,81,113,117,229,93,187,107,37,174,159,54,41,203,153,237,96,162,138,43,188,128,162,138,40,0,162,138,40,0,175,153,255,0,107,143,217,31,224,143,237,183,240,71,196,159,1,126,61,120,110,77,107,194,154,212,144,234,218,30,185,164,205,111,167,120,215,225,207,141,116,235,123,200,60,61,241,27,225,207,136,103,179,159,254,17,207,26,233,223,111,189,88,102,104,110,45,47,45,53,11,221,35,87,178,212,180,61,75,83,211,47,74,40,105,61,26,186,3,249,11,248,253,255,0,4,155,255,0,130,138,126,195,6,29,107,225,141,181,199,237,155,240,114,193,172,85,188,75,240,99,195,26,141,151,197,237,22,73,223,192,186,24,111,18,252,0,254,212,212,181,77,74,57,188,67,226,45,120,218,31,9,223,248,165,160,211,60,51,119,172,107,179,248,122,215,247,67,231,207,16,254,212,159,30,63,103,180,176,79,218,7,224,207,198,127,129,41,226,165,157,60,30,62,48,252,48,241,255,0,194,195,226,133,240,240,183,26,232,240,243,120,167,71,182,58,247,216,70,179,162,125,176,195,230,125,155,251,90,219,205,42,110,99,220,81,94,86,39,1,134,147,114,228,179,118,219,205,175,47,34,227,39,162,232,73,164,252,123,253,166,255,0,104,45,38,239,196,223,2,63,103,191,218,15,227,79,131,237,39,151,195,58,143,139,62,18,124,25,248,153,241,47,195,186,111,137,236,225,131,83,186,208,111,53,239,11,232,115,195,103,174,65,167,106,250,45,212,150,109,42,76,144,106,118,211,50,132,184,66,223,92,254,205,191,240,68,239,219,103,246,204,143,72,241,191,237,101,226,75,191,217,31,224,222,176,250,86,180,60,29,61,189,151,137,127,105,15,23,104,58,164,126,6,241,4,246,203,225,163,55,246,111,193,167,188,240,254,179,226,235,20,159,196,77,115,226,31,15,107,186,17,135,90,240,5,220,4,73,33,69,24,92,6,26,28,178,81,109,218,250,252,188,132,228,217,253,130,124,24,248,49,240,183,246,120,248,91,224,175,130,191,5,124,21,163,252,60,248,95,240,243,71,77,15,194,94,18,208,210,127,178,105,246,158,124,247,183,151,87,87,151,179,205,117,173,235,183,218,173,221,253,254,169,170,95,207,117,169,106,218,150,167,119,169,234,119,119,119,247,119,55,50,250,117,20,87,171,182,196,133,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,31,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 8969; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

