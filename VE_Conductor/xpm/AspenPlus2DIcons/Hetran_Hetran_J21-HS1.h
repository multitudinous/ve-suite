#ifndef GETVESUITE_Hetran_Hetran_J21-HS1_H
#define GETVESUITE_Hetran_Hetran_J21-HS1_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Hetran_Hetran_J21-HS1( void )
{
    unsigned char osgData[ 5558 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,60,0,147,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,237,188,75,241,115,225,71,130,207,143,87,198,63,19,190,30,248,77,190,21,248,2,203,226,199,196,241,226,95,26,120,111,66,63,14,62,22,106,77,227,53,211,190,37,120,244,106,154,148,95,240,135,248,2,118,248,113,241,12,67,172,234,31,103,211,165,62,3,214,130,92,183,246,93,247,145,232,85,252,21,124,106,255,0,130,212,39,252,20,35,225,119,197,159,27,252,69,253,146,254,33,124,4,215,252,123,255,0,4,202,253,182,191,100,159,128,186,15,195,239,28,105,63,180,15,134,252,99,172,126,223,26,23,192,141,106,211,199,31,20,188,95,226,31,12,252,61,184,248,89,162,120,98,247,246,126,240,140,31,103,209,244,95,26,205,173,193,241,31,80,186,15,164,201,225,219,107,79,18,125,75,164,255,0,193,213,223,30,117,173,67,197,26,101,151,252,18,183,66,55,94,16,215,109,252,59,172,9,63,110,13,121,85,53,27,159,13,120,119,197,145,172,59,63,98,86,38,63,236,159,20,105,100,239,8,219,221,128,82,155,29,249,227,136,131,157,85,41,69,66,22,179,190,247,222,254,143,65,217,164,159,115,251,48,175,202,15,219,215,194,127,0,191,106,95,31,127,193,48,252,11,227,175,12,252,31,253,163,62,13,235,159,240,81,255,0,140,30,19,241,143,131,188,89,163,120,47,226,247,195,61,95,197,95,9,255,0,224,158,159,240,83,93,31,196,62,25,241,55,135,181,139,109,67,74,212,60,65,225,175,138,222,6,188,181,189,178,186,133,238,116,111,16,248,58,72,39,142,219,82,211,217,97,236,191,224,150,223,240,84,95,128,159,240,85,31,217,215,74,248,205,240,161,39,240,71,196,61,15,78,240,204,63,28,126,0,120,143,84,181,212,60,117,240,99,197,30,37,211,38,212,116,132,185,186,130,218,220,120,191,225,198,179,5,150,169,115,225,95,21,91,90,219,217,235,182,154,101,229,188,246,186,79,136,244,127,18,120,115,66,252,15,255,0,130,150,126,211,63,25,63,101,79,248,41,191,193,223,216,231,225,79,131,188,65,224,45,39,227,231,237,191,251,52,252,119,253,154,254,61,248,99,78,183,248,135,240,207,246,105,248,201,255,0,5,28,253,150,255,0,224,160,191,240,78,239,137,62,51,241,71,134,60,65,224,253,59,195,126,11,215,227,248,239,121,63,198,173,11,225,98,218,107,209,252,79,241,15,133,254,48,248,187,196,95,16,108,47,60,107,113,163,248,71,160,71,244,61,255,0,14,158,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,215,255,0,9,190,22,248,19,224,119,194,207,134,159,5,126,22,232,95,240,139,252,50,248,63,240,255,0,193,159,11,126,29,120,107,251,79,88,214,255,0,225,29,240,39,195,255,0,14,105,222,19,240,134,133,253,181,226,45,66,239,80,213,254,201,225,253,39,79,183,251,85,245,221,213,228,255,0,103,243,110,110,38,157,228,145,189,2,128,62,0,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,143,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,107,239,250,40,3,224,15,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,190,255,0,162,128,62,0,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,143,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,107,239,250,40,3,224,15,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,63,240,73,239,248,37,144,255,0,156,105,254,192,31,248,134,255,0,179,175,255,0,59,154,251,254,138,0,252,65,241,199,236,157,251,2,252,31,253,168,255,0,224,150,223,20,191,100,63,217,167,246,64,248,91,226,11,159,219,255,0,227,239,194,127,16,124,70,253,155,254,13,252,24,240,70,177,113,255,0,8,135,252,19,183,254,10,95,225,223,136,255,0,13,53,143,23,124,49,240,221,180,243,127,101,252,77,248,127,45,142,183,163,79,112,126,197,175,120,37,173,111,237,162,212,52,211,28,63,183,213,252,128,127,193,116,62,59,124,117,255,0,130,112,252,79,248,102,159,178,223,133,135,138,245,223,140,63,16,127,108,47,248,40,23,236,233,162,89,107,126,16,209,109,191,103,255,0,138,255,0,8,63,224,153,223,182,111,194,143,219,171,226,4,58,87,141,244,87,240,197,143,195,237,62,211,227,167,193,31,142,154,55,133,97,209,245,173,71,226,31,196,123,143,142,147,248,143,80,134,255,0,197,190,29,130,231,250,159,240,159,134,126,13,126,202,31,0,188,51,224,237,30,231,195,255,0,8,255,0,103,207,217,175,224,254,141,225,157,46,243,197,158,44,158,223,194,191,12,126,13,124,28,240,93,182,151,99,115,226,111,29,120,235,89,150,88,60,63,162,248,39,195,145,53,238,173,172,234,18,72,182,218,108,151,154,133,235,176,154,114,175,171,93,128,246,10,43,249,6,241,151,252,29,97,46,185,241,51,226,158,147,251,45,126,192,90,151,199,79,130,158,6,241,171,120,59,194,95,23,252,123,251,68,107,159,3,53,239,136,176,219,120,111,195,154,236,190,44,139,225,64,253,152,252,79,117,224,239,14,222,29,121,38,210,33,213,53,56,245,153,180,185,109,46,181,157,35,64,212,231,186,209,52,254,35,194,223,240,117,127,199,159,23,248,103,195,190,45,209,255,0,224,149,186,27,233,30,40,208,180,159,17,105,109,47,237,191,175,9,155,78,214,180,251,125,74,197,165,88,191,98,103,65,33,182,186,136,157,142,235,156,237,118,24,39,55,90,146,189,234,37,109,55,238,52,155,217,31,217,133,21,252,31,252,74,255,0,131,213,188,73,240,163,198,218,215,128,60,89,255,0,4,182,183,143,95,208,63,179,190,222,150,159,182,221,195,91,143,237,77,38,195,90,180,242,218,239,246,62,134,67,155,45,70,216,157,209,47,204,78,55,46,24,149,162,105,164,211,186,98,56,239,216,211,246,94,155,95,253,143,255,0,101,61,120,105,41,40,214,255,0,102,207,129,154,176,144,136,51,34,234,95,11,252,47,120,28,147,48,60,137,129,231,29,105,126,6,126,203,211,106,95,19,255,0,108,187,63,236,152,219,254,17,255,0,218,75,194,218,67,2,33,253,209,159,246,64,253,148,181,239,44,3,48,206,70,184,27,3,60,191,99,211,247,139,254,9,171,240,40,234,255,0,240,78,111,216,11,86,22,177,183,246,167,236,81,251,43,106,59,136,108,176,189,248,23,224,59,156,241,17,228,137,121,235,207,122,63,101,111,129,77,125,241,211,254,10,83,109,246,104,201,209,255,0,109,127,2,105,196,97,200,95,55,254,9,205,251,1,234,216,0,71,255,0,81,76,227,31,197,211,215,225,126,184,221,92,108,111,126,89,175,198,170,76,234,80,248,117,213,163,249,36,253,135,126,7,124,86,240,255,0,195,255,0,129,159,17,254,8,252,71,248,137,240,119,199,154,231,236,199,224,11,107,223,26,252,40,241,199,137,62,26,120,186,247,194,254,37,240,223,195,157,111,85,240,213,215,136,252,39,127,107,117,119,225,235,141,67,70,240,157,213,197,153,144,219,205,115,162,217,207,50,188,214,176,180,127,160,154,159,134,127,109,157,119,192,254,47,240,31,136,127,104,127,137,222,38,181,241,215,136,62,15,120,155,196,158,52,241,78,189,167,248,155,227,85,198,165,251,62,124,73,177,248,197,240,50,223,77,253,161,53,221,26,227,199,158,30,208,60,37,241,78,202,127,16,120,127,74,211,188,71,105,165,105,122,175,136,117,187,203,75,40,229,241,6,184,218,143,232,23,252,18,19,246,122,139,198,127,179,87,236,159,168,220,105,242,58,221,254,196,159,3,53,0,84,54,231,123,175,0,124,47,147,205,50,36,89,218,99,145,0,83,144,54,240,121,175,216,207,248,100,123,15,250,5,207,255,0,125,79,255,0,198,42,171,230,181,40,214,169,77,84,178,140,191,95,243,20,97,116,182,208,254,92,127,177,191,224,161,63,244,124,191,182,143,254,37,111,198,159,254,107,168,254,198,255,0,130,133,127,209,243,126,218,63,248,149,191,26,127,249,174,175,234,59,254,25,30,195,254,129,115,255,0,223,83,255,0,241,138,63,225,145,236,63,232,23,63,253,245,63,255,0,24,172,86,117,85,255,0,203,199,167,159,161,94,207,208,254,92,127,177,191,224,161,95,244,124,223,182,143,254,37,111,198,159,254,107,168,254,198,255,0,130,133,127,209,243,126,218,63,248,149,191,26,127,249,174,175,234,59,254,25,30,195,254,129,115,255,0,223,83,255,0,241,138,63,225,145,236,63,232,23,63,253,245,63,255,0,24,164,179,154,175,254,94,61,60,253,3,217,250,31,203,143,246,55,252,20,43,254,143,155,246,209,255,0,196,173,248,211,255,0,205,117,31,216,223,240,80,175,250,62,111,219,71,255,0,18,183,227,79,255,0,53,213,253,71,127,195,35,216,127,208,46,127,251,234,127,254,49,71,252,50,61,135,253,2,231,255,0,190,167,255,0,227,20,44,230,171,255,0,151,143,79,63,64,246,126,135,242,227,253,141,255,0,5,10,255,0,163,230,253,180,127,241,43,126,52,255,0,243,93,71,246,55,252,20,43,254,143,155,246,209,255,0,196,173,248,211,255,0,205,117,127,81,223,240,200,246,31,244,11,159,254,250,159,255,0,140,81,255,0,12,143,97,255,0,64,185,255,0,239,169,255,0,248,197,11,57,170,255,0,229,227,211,207,208,61,159,161,252,184,255,0,99,127,193,66,191,232,249,191,109,31,252,74,223,141,63,252,215,81,253,141,255,0,5,10,255,0,163,230,253,180,127,241,43,126,52,255,0,243,93,95,212,119,252,50,61,135,253,2,231,255,0,190,167,255,0,227,20,127,195,35,216,127,208,46,127,251,234,127,254,49,66,206,170,244,168,244,243,244,15,103,232,127,47,63,216,95,182,229,231,135,254,36,120,103,197,95,180,47,196,127,137,218,111,197,111,133,30,59,248,25,226,251,143,141,154,166,139,241,219,95,79,132,63,20,236,236,236,126,39,252,61,240,127,139,62,50,104,58,238,171,240,215,195,62,36,131,74,240,243,107,240,120,110,243,73,26,204,254,15,208,110,181,19,115,115,160,104,178,216,120,95,199,191,135,31,181,231,143,60,21,99,225,31,140,63,181,31,237,57,241,91,225,246,173,226,157,10,109,103,225,255,0,196,159,218,15,226,175,142,60,9,226,11,221,10,75,143,18,248,112,248,143,194,26,247,138,38,176,215,82,203,196,218,46,143,168,217,45,220,19,37,174,167,164,88,223,219,172,119,182,150,183,16,255,0,95,95,240,200,246,31,244,11,159,254,250,159,255,0,140,87,205,191,181,23,236,173,107,167,120,7,68,185,131,78,146,57,23,198,154,104,86,117,146,64,8,209,188,65,32,249,100,136,2,119,70,61,122,99,21,116,243,138,146,146,92,239,93,52,249,110,39,78,202,246,191,200,254,104,127,97,223,217,121,181,53,253,168,44,173,244,196,158,31,14,254,209,154,86,143,17,217,20,97,34,151,246,91,253,153,181,216,163,8,204,132,1,22,180,135,145,147,191,146,88,154,245,127,216,215,246,95,159,196,31,178,15,236,167,175,46,144,146,13,111,246,109,248,27,171,137,8,131,46,53,63,134,30,22,188,15,204,163,168,159,61,1,231,166,120,175,216,47,248,38,215,192,246,213,53,191,219,250,33,102,170,52,159,219,91,195,250,111,150,249,102,140,31,216,43,246,26,212,149,65,17,116,198,162,8,28,96,55,208,215,176,127,193,53,126,5,182,175,255,0,4,230,253,128,245,97,109,25,254,212,253,138,63,101,109,71,36,49,45,246,223,129,94,4,185,201,62,95,83,230,158,253,251,245,171,197,99,37,21,81,222,222,253,53,247,211,184,163,6,249,91,211,127,205,31,230,133,255,0,5,119,240,139,120,19,254,10,31,251,66,120,85,224,22,205,165,127,194,167,204,35,110,19,237,223,3,254,26,106,35,27,88,140,17,120,15,4,253,234,43,232,159,248,56,163,195,199,194,159,240,88,239,219,11,64,42,19,236,31,240,207,191,34,140,5,251,87,236,181,240,70,247,0,96,96,127,164,122,81,95,93,134,168,222,27,14,251,194,15,255,0,37,71,59,209,181,216,255,0,72,239,248,38,31,198,107,13,47,254,9,171,255,0,4,242,211,29,151,126,157,251,13,254,201,118,15,196,92,53,167,192,79,0,91,156,146,254,177,158,188,241,71,236,153,241,154,194,211,227,223,252,20,242,225,153,64,213,127,110,79,0,95,166,68,95,114,63,248,38,175,252,19,207,76,36,124,255,0,223,211,159,216,227,235,143,196,159,216,107,246,156,139,64,253,137,191,99,205,8,203,131,162,254,203,95,179,238,144,71,218,138,145,253,157,240,151,194,86,100,99,200,59,112,97,233,147,142,148,191,179,239,237,57,22,155,241,107,246,229,188,50,96,107,255,0,181,47,132,117,96,126,211,183,34,31,216,155,246,60,208,179,159,35,230,231,69,110,125,177,206,50,126,51,234,146,246,216,231,107,115,206,46,223,247,22,44,233,83,95,187,190,246,253,15,183,191,224,144,63,29,108,60,23,251,39,126,199,150,109,119,5,151,147,251,14,126,207,214,82,59,152,163,102,184,131,225,159,194,244,117,98,177,100,146,145,70,195,112,36,142,119,28,87,236,7,252,53,110,157,255,0,65,187,111,251,254,159,252,98,191,134,143,217,171,246,199,177,248,71,251,54,126,204,182,215,26,172,112,181,159,236,235,240,115,195,140,130,105,226,216,214,95,14,188,38,202,172,214,227,113,127,46,4,82,8,192,242,177,184,224,87,183,127,195,205,116,143,250,13,199,255,0,129,186,151,183,253,51,247,31,157,42,249,100,234,214,171,53,6,212,164,255,0,48,140,249,82,91,223,254,1,253,148,255,0,195,86,233,223,244,27,182,255,0,191,233,255,0,198,40,255,0,134,173,211,191,232,55,109,255,0,127,211,255,0,140,87,241,173,255,0,15,52,210,63,232,55,31,254,6,234,95,252,110,143,248,121,166,145,255,0,65,184,255,0,240,55,82,255,0,227,117,146,202,42,127,207,182,190,94,159,215,200,126,211,208,254,202,127,225,171,116,239,250,13,219,127,223,244,255,0,227,20,127,195,86,233,223,244,27,182,255,0,191,233,255,0,198,43,248,214,255,0,135,154,105,31,244,27,143,255,0,3,117,47,254,55,71,252,60,211,72,255,0,160,220,127,248,27,169,127,241,186,22,81,83,254,125,181,242,244,254,190,65,237,61,15,236,167,254,26,183,78,255,0,160,221,183,253,255,0,79,254,49,71,252,53,110,157,255,0,65,187,111,251,254,159,252,98,191,141,111,248,121,166,145,255,0,65,184,255,0,240,55,82,255,0,227,116,127,195,205,52,143,250,13,199,255,0,129,186,151,255,0,27,163,251,30,166,150,131,243,211,208,61,167,145,253,148,255,0,195,86,233,223,244,27,182,255,0,191,233,255,0,198,40,255,0,134,173,211,191,232,55,109,255,0,127,211,255,0,140,87,241,173,255,0,15,52,210,63,232,55,31,254,6,234,95,252,110,143,248,121,166,145,255,0,65,184,255,0,240,55,82,255,0,227,116,44,162,167,252,251,107,229,233,253,124,131,218,122,31,217,79,252,53,110,157,255,0,65,187,111,251,254,159,252,98,143,248,106,221,59,254,131,118,223,247,253,63,248,197,127,26,223,240,243,77,35,254,131,113,255,0,224,110,165,255,0,198,232,255,0,135,154,105,31,244,27,143,255,0,3,117,47,254,55,66,202,42,127,207,182,190,94,159,215,200,61,167,161,253,148,255,0,195,86,233,223,244,27,182,255,0,191,233,255,0,198,43,231,239,218,67,246,155,176,213,188,19,163,218,71,169,219,92,185,241,118,158,235,31,153,27,252,199,73,215,34,86,33,163,80,62,105,0,206,71,222,175,229,95,254,30,105,164,127,208,110,63,252,13,212,191,248,221,101,106,95,240,80,125,47,198,147,232,186,36,122,220,123,159,87,73,178,46,174,228,43,254,129,127,111,184,45,192,80,112,110,7,66,15,97,198,106,161,148,212,140,163,46,70,173,229,233,253,124,129,212,186,178,63,163,143,248,38,175,197,253,55,74,241,55,252,20,69,129,69,143,82,253,184,252,55,168,69,133,133,65,140,127,193,62,63,96,171,0,192,0,191,41,123,22,35,3,28,231,156,230,189,119,254,9,135,241,154,195,74,255,0,130,106,255,0,193,60,244,185,25,67,233,223,176,223,236,153,96,224,136,179,186,211,224,31,128,45,219,171,231,172,103,175,227,205,127,63,159,176,199,237,49,6,143,168,126,215,247,34,225,100,79,16,126,212,26,22,181,27,165,199,150,173,24,253,143,255,0,100,237,30,49,129,1,25,81,164,109,56,192,5,72,29,50,125,59,246,26,253,167,99,240,255,0,236,79,251,30,232,70,64,14,139,251,45,126,207,186,73,255,0,73,218,1,211,126,18,248,70,204,141,190,65,199,250,163,198,120,233,158,245,174,47,11,38,170,245,110,116,223,221,78,194,140,213,163,117,209,254,135,242,129,255,0,7,36,107,177,120,155,254,11,75,251,102,235,112,144,99,189,255,0,134,118,218,70,49,254,141,251,40,124,11,180,111,186,113,247,173,205,21,224,95,240,90,159,22,143,28,255,0,193,76,191,105,95,20,169,200,213,63,225,77,224,239,50,127,199,143,192,15,133,122,119,46,84,110,57,179,244,20,87,216,225,97,37,134,195,171,109,78,31,250,74,57,158,173,159,173,63,10,60,39,255,0,5,52,240,31,194,207,134,190,6,155,254,9,199,255,0,5,2,73,124,23,240,255,0,193,190,19,149,7,236,67,251,82,200,22,79,14,248,119,77,209,221,4,145,124,53,41,38,26,204,141,203,242,156,100,112,104,240,119,132,255,0,224,166,190,29,241,23,197,125,97,191,224,156,159,240,80,37,95,30,248,255,0,77,241,100,71,254,24,135,246,165,109,233,103,240,187,225,175,129,139,237,79,134,164,198,68,158,11,145,64,127,152,236,221,247,25,107,253,79,40,171,250,149,15,123,221,248,245,127,125,255,0,49,243,61,60,143,229,155,254,13,196,255,0,130,76,120,187,246,107,248,39,224,95,218,223,246,204,248,89,121,225,95,218,179,88,240,94,139,225,79,129,223,15,252,95,120,210,235,63,179,199,192,230,240,15,134,180,89,117,13,127,192,178,233,81,47,128,191,104,111,20,223,143,20,255,0,108,27,155,173,67,86,208,188,33,117,165,120,108,199,225,77,103,83,248,139,161,106,31,212,205,20,87,74,74,42,201,89,18,20,81,69,48,10,40,162,128,10,252,124,253,146,199,237,85,113,251,64,254,206,186,255,0,196,131,251,64,47,194,223,28,124,58,255,0,130,206,124,74,190,183,248,128,126,34,218,232,214,58,7,197,15,248,41,71,236,229,241,27,246,9,211,190,39,120,111,197,5,95,225,247,196,27,111,217,19,197,90,221,191,132,124,41,226,123,93,51,197,62,16,208,109,124,71,225,99,163,104,147,232,222,33,210,44,127,96,232,164,210,110,46,239,221,119,223,202,218,247,220,2,138,40,166,1,69,20,80,1,95,28,126,222,191,177,111,195,79,219,231,246,97,248,143,251,58,252,68,183,210,52,253,71,94,210,53,45,91,225,47,196,171,237,6,111,16,107,31,2,254,53,217,104,122,198,159,240,235,227,95,132,236,236,181,221,42,230,125,111,67,212,245,105,154,230,194,45,82,194,223,196,26,53,254,171,225,109,98,89,180,13,119,86,179,186,251,30,138,55,220,15,242,240,186,253,151,63,224,170,159,178,15,197,63,143,127,7,124,89,251,10,126,212,159,16,117,155,47,138,150,250,140,159,16,62,1,254,207,95,28,190,58,124,27,241,213,172,31,11,190,26,248,107,78,241,135,195,191,137,223,15,252,5,61,143,136,60,57,168,216,120,114,222,234,20,184,91,61,102,192,220,182,157,226,13,43,70,215,45,53,29,38,206,135,194,223,135,191,240,81,79,134,255,0,179,22,159,173,107,223,178,87,198,47,9,105,255,0,1,52,31,217,215,225,111,139,252,5,227,111,12,205,224,143,143,176,106,191,22,53,255,0,14,124,15,248,41,46,131,251,50,120,175,83,178,248,141,227,109,3,197,191,20,37,159,195,190,27,213,244,79,10,106,26,70,183,172,248,115,93,211,180,219,219,139,175,15,107,177,105,191,234,61,95,203,7,252,20,83,227,47,138,52,95,248,44,168,253,158,45,108,52,25,60,21,241,55,246,98,255,0,130,24,252,109,215,181,75,139,93,65,252,81,105,226,191,128,191,240,113,71,195,239,0,248,63,79,210,47,99,213,18,210,223,195,247,58,63,237,45,227,185,53,40,166,178,184,185,154,231,73,210,94,214,238,206,40,47,33,191,227,171,133,164,211,124,183,114,148,47,175,154,143,229,183,153,74,77,91,200,254,9,255,0,106,47,248,39,199,252,21,27,227,135,199,95,28,252,81,182,255,0,130,107,255,0,193,64,39,131,196,255,0,240,140,236,148,254,198,63,180,149,177,111,236,79,7,248,127,195,173,152,110,190,27,137,19,15,164,176,249,186,227,35,130,40,175,246,119,162,186,163,21,20,162,182,138,183,220,73,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5558; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

