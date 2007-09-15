#ifndef GETVESUITE_Aerotran_Aerotran_ECN-HS-2_H
#define GETVESUITE_Aerotran_Aerotran_ECN-HS-2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Aerotran_Aerotran_ECN-HS-2( void )
{
    unsigned char osgData[ 4780 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,57,0,151,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,138,253,165,63,224,145,223,5,190,34,127,193,71,188,27,225,143,24,252,78,241,31,140,225,253,169,62,16,254,222,31,181,71,141,252,71,241,87,246,87,255,0,130,93,252,127,248,147,225,239,20,124,44,248,225,251,29,104,255,0,15,254,29,120,79,226,223,237,25,255,0,4,251,241,127,138,174,254,16,104,190,24,253,167,60,87,161,232,218,79,136,53,157,126,231,67,240,239,129,60,37,225,237,19,81,211,116,93,9,44,110,59,127,248,33,87,236,21,240,167,193,190,31,178,253,189,109,117,104,34,248,191,169,220,254,221,159,178,116,158,27,240,47,236,247,251,16,126,206,255,0,13,79,195,175,5,126,220,218,199,128,180,221,103,83,210,63,101,15,217,71,192,58,167,140,60,106,250,63,236,171,240,238,81,115,175,235,26,181,141,141,206,183,226,23,209,244,221,50,45,89,225,135,242,132,127,193,193,63,180,47,198,31,142,223,179,151,237,141,163,127,193,56,188,41,99,163,120,71,246,96,248,253,240,215,72,240,101,199,237,143,226,91,139,143,16,233,159,181,79,139,63,100,175,137,214,30,41,159,196,48,126,198,219,52,121,180,123,79,217,190,59,87,177,75,75,213,191,111,27,153,214,254,208,105,130,61,67,158,253,146,63,224,189,159,180,55,236,69,240,83,192,31,179,197,247,252,19,183,194,127,16,174,188,79,241,215,246,151,241,46,139,226,136,255,0,108,15,18,120,78,57,181,63,142,191,24,191,104,15,218,210,109,18,227,69,127,216,239,81,48,195,164,104,222,33,213,244,145,117,29,212,207,127,38,132,151,198,210,198,59,167,183,180,226,141,85,245,170,183,171,23,77,69,89,93,104,250,149,202,236,157,191,173,15,238,218,138,254,69,127,226,37,255,0,218,107,254,145,109,224,223,252,78,111,21,126,95,242,98,212,127,196,75,255,0,180,215,253,34,219,193,191,248,156,254,42,255,0,232,22,173,190,179,135,255,0,159,209,251,195,150,93,143,235,170,138,254,69,127,226,37,255,0,218,107,254,145,109,224,223,252,78,127,21,127,244,11,81,255,0,17,47,254,211,95,244,139,111,6,255,0,226,115,248,171,255,0,160,90,143,172,208,255,0,159,209,251,195,150,93,143,235,170,138,254,69,127,226,37,255,0,218,107,254,145,109,224,223,252,78,127,21,127,244,11,81,255,0,17,47,254,211,95,244,139,111,6,255,0,226,115,120,171,255,0,160,90,143,172,225,255,0,231,244,126,244,46,89,118,63,172,175,22,120,179,194,190,2,240,175,137,124,117,227,175,18,248,127,193,126,9,240,95,135,245,159,22,120,199,198,62,44,214,116,239,14,120,87,194,126,21,240,230,157,115,172,120,135,196,190,37,241,14,177,115,13,166,133,225,251,13,34,206,238,234,246,246,234,104,173,173,109,173,100,158,121,18,36,102,24,94,41,248,171,240,191,192,214,222,19,189,241,183,196,143,1,120,58,207,199,190,33,240,215,132,188,13,119,226,159,24,120,123,195,246,222,51,241,95,141,53,109,51,65,240,119,134,60,39,62,173,168,194,158,34,241,14,173,174,107,90,53,150,153,101,102,102,185,191,187,213,173,173,173,98,150,105,226,71,254,52,255,0,107,15,248,47,103,237,53,251,80,126,203,63,180,183,236,209,255,0,14,222,240,111,129,255,0,225,162,62,0,124,100,248,25,255,0,9,175,252,54,95,138,188,76,124,31,255,0,11,111,225,215,136,252,3,255,0,9,79,252,35,127,240,197,186,127,252,36,31,217,255,0,240,144,125,175,236,63,111,178,251,95,217,60,143,182,91,121,158,114,120,95,237,77,255,0,5,129,253,166,127,105,111,13,254,202,30,31,255,0,134,5,240,127,130,207,236,193,251,65,254,205,95,29,254,216,127,107,31,21,120,143,254,19,143,248,103,159,138,255,0,13,62,39,31,11,24,63,225,145,108,63,225,27,254,216,31,14,205,143,219,183,234,31,217,255,0,219,63,106,251,29,239,217,254,207,60,203,21,69,90,213,34,239,110,190,105,63,185,54,198,161,38,175,109,17,253,238,209,95,200,175,252,68,191,251,77,127,210,45,188,27,255,0,137,205,226,175,254,129,122,63,226,37,255,0,218,107,254,145,109,224,223,252,78,127,21,127,244,11,85,253,98,130,209,213,141,253,67,150,93,143,235,170,138,254,69,127,226,37,255,0,218,107,254,145,109,224,223,252,78,127,21,127,244,11,81,255,0,17,47,254,211,95,244,139,111,6,255,0,226,115,248,171,255,0,160,90,151,214,104,127,207,232,253,225,203,46,199,245,213,69,127,34,191,241,18,255,0,237,53,255,0,72,182,240,111,254,39,63,138,191,250,5,168,255,0,136,151,255,0,105,175,250,69,183,131,127,241,57,252,85,255,0,208,45,71,214,104,127,207,232,253,225,203,46,199,245,213,69,127,34,191,241,18,255,0,237,53,255,0,72,182,240,111,254,39,63,138,187,244,255,0,155,22,174,119,68,255,0,131,161,255,0,104,125,127,82,241,126,149,97,255,0,4,182,240,153,187,240,71,136,173,124,49,173,137,127,110,95,18,132,26,157,223,132,252,47,227,56,133,185,143,246,29,98,240,255,0,99,120,191,72,36,186,198,254,97,144,4,49,132,146,70,177,20,90,109,85,141,151,152,185,101,216,254,195,40,175,227,71,226,199,252,29,95,241,207,224,207,128,53,255,0,137,62,49,255,0,130,91,248,105,124,57,225,193,165,29,68,233,255,0,183,31,136,37,187,255,0,137,198,181,166,232,54,126,74,95,126,196,118,241,31,244,253,86,215,118,233,147,228,220,87,115,5,86,43,72,78,19,87,132,148,151,144,154,182,231,57,251,10,126,199,118,126,39,253,136,191,99,143,18,182,142,178,183,136,127,101,111,217,235,92,105,126,202,27,205,109,91,225,31,132,111,217,247,125,132,238,201,184,206,114,115,187,169,235,79,253,161,63,99,171,61,39,226,215,236,45,96,186,56,67,226,111,218,163,197,250,33,2,215,30,96,183,253,137,63,108,95,18,4,199,216,70,236,55,135,131,99,13,247,50,7,27,151,247,51,254,9,115,161,252,60,155,254,9,153,255,0,4,235,154,246,112,183,178,126,194,159,178,52,151,99,236,209,54,46,159,246,127,248,124,215,3,113,79,155,247,165,185,239,222,159,251,92,104,95,15,99,248,253,255,0,4,188,88,103,253,212,191,183,87,196,24,239,136,182,137,74,218,175,252,19,59,254,10,39,42,28,108,195,31,182,71,103,193,7,174,122,142,62,10,56,170,139,49,173,78,238,203,218,63,43,164,236,117,242,254,238,54,90,233,250,31,154,223,240,195,246,95,244,4,82,70,121,251,32,228,118,255,0,152,127,165,31,240,195,246,95,244,4,95,252,4,31,252,175,175,232,75,254,17,239,134,156,159,180,158,79,252,250,69,232,51,143,147,159,254,189,31,240,143,124,51,255,0,159,159,252,149,139,255,0,136,175,47,235,181,147,210,247,127,240,11,228,137,252,246,255,0,195,15,217,127,208,17,127,240,16,127,242,190,143,248,97,251,47,250,2,47,254,2,15,254,87,215,244,37,255,0,8,247,195,63,249,249,255,0,201,88,191,248,138,63,225,30,248,103,255,0,63,63,249,43,23,255,0,17,77,99,171,233,191,245,111,235,238,14,72,159,207,111,252,48,253,151,253,1,23,255,0,1,7,255,0,43,232,255,0,134,31,178,255,0,160,34,255,0,224,32,255,0,229,125,127,66,95,240,143,124,51,255,0,159,159,252,149,139,255,0,136,163,254,17,239,134,127,243,243,255,0,146,177,127,241,20,125,122,182,155,223,254,24,57,34,127,61,191,240,195,246,95,244,4,95,252,4,31,252,175,163,254,24,126,203,254,128,139,255,0,128,131,255,0,149,245,253,9,127,194,61,240,207,254,126,127,242,86,47,254,34,143,248,71,190,25,255,0,207,207,254,74,197,255,0,196,82,88,218,218,111,166,191,144,114,35,249,237,255,0,134,31,178,255,0,160,34,255,0,224,32,255,0,229,125,31,240,195,246,95,244,4,95,252,4,31,252,175,175,232,75,254,17,239,134,127,243,243,255,0,146,177,127,241,20,127,194,61,240,207,254,126,127,242,86,47,254,34,159,215,171,111,118,223,252,48,185,34,127,61,191,240,195,246,95,244,4,95,252,4,31,252,175,163,254,24,126,203,254,128,139,255,0,128,131,255,0,149,245,253,9,127,194,61,240,207,254,126,127,242,86,47,254,34,143,248,71,190,25,255,0,207,207,254,74,197,255,0,196,80,177,213,244,223,250,183,245,247,15,146,39,243,219,255,0,12,63,101,255,0,64,69,255,0,192,65,255,0,202,250,63,225,135,236,191,232,8,191,248,8,63,249,95,95,208,151,252,35,223,12,255,0,231,231,255,0,37,98,255,0,226,40,255,0,132,123,225,159,252,252,255,0,228,172,95,252,69,11,29,95,77,255,0,171,127,95,112,114,68,254,123,71,236,63,100,14,70,136,185,60,2,45,7,161,198,113,96,61,79,233,95,44,254,207,159,177,221,166,173,241,111,246,232,176,109,29,91,254,17,159,218,155,194,26,24,2,212,31,40,79,251,18,126,199,94,37,217,131,98,113,150,241,3,49,4,15,191,157,167,36,159,234,227,254,17,239,134,103,31,233,60,3,207,250,44,62,135,213,127,206,43,225,127,217,35,66,248,123,39,199,239,248,42,34,220,92,1,12,63,183,79,195,248,236,191,209,162,57,182,63,240,76,239,248,39,108,142,72,218,118,55,219,100,187,227,140,237,207,83,207,102,31,23,86,88,108,92,155,119,132,98,255,0,242,104,162,92,87,52,126,103,243,5,255,0,5,177,253,149,173,126,27,127,193,49,255,0,105,159,26,69,165,11,102,209,143,193,172,79,246,97,30,207,237,31,218,11,225,78,149,247,197,154,224,149,189,35,239,2,115,131,158,132,175,216,255,0,248,57,111,71,240,53,167,252,17,51,246,212,184,209,230,223,168,167,252,51,144,183,79,179,199,24,109,223,181,167,192,116,151,230,84,5,113,3,203,208,251,81,95,83,145,86,148,240,17,148,213,223,51,252,145,207,85,37,55,99,230,223,216,15,246,180,180,240,231,236,39,251,21,120,121,181,36,70,208,191,100,175,217,195,70,40,91,27,14,151,240,119,193,182,5,15,250,33,198,13,185,238,126,167,173,59,246,141,253,173,173,117,79,140,63,176,53,242,234,74,223,240,141,254,214,126,50,214,88,131,247,22,127,216,83,246,212,240,246,252,139,65,129,157,117,71,124,111,232,51,145,248,183,109,255,0,4,253,255,0,130,206,126,207,190,51,248,101,251,28,65,251,35,143,28,120,195,74,248,85,227,171,143,134,250,223,131,127,104,31,217,166,31,7,252,80,248,99,251,50,234,127,8,254,24,120,219,226,111,133,111,60,107,241,51,72,190,209,124,52,218,151,197,159,133,83,88,216,248,146,199,66,241,29,197,175,141,34,103,209,34,154,195,87,135,79,227,252,3,251,22,127,193,103,191,106,191,15,124,44,248,213,240,131,246,34,241,15,139,124,13,224,143,140,31,21,97,179,214,159,246,134,253,145,188,62,215,94,42,248,99,255,0,11,175,246,102,248,133,164,65,166,120,155,227,109,149,220,114,233,222,62,30,45,178,243,228,183,22,215,105,163,189,213,140,215,54,115,218,92,205,130,203,37,245,186,181,57,84,155,230,237,123,73,127,193,45,78,241,73,189,21,191,11,31,212,65,253,179,236,206,63,226,104,156,12,125,238,15,184,255,0,67,228,99,28,210,127,195,103,89,255,0,208,81,63,239,161,255,0,200,117,248,7,255,0,14,238,255,0,130,248,127,210,61,188,75,255,0,137,63,251,16,240,59,15,249,56,79,74,63,225,221,223,240,95,15,250,71,183,137,127,241,39,255,0,98,31,254,136,74,227,254,193,169,111,134,214,243,94,69,123,95,52,126,254,127,195,103,89,255,0,208,81,63,239,175,254,227,163,254,27,58,207,254,130,137,255,0,125,127,247,29,126,1,255,0,195,187,191,224,190,31,244,143,111,18,255,0,226,79,254,196,63,253,16,148,127,195,187,191,224,190,31,244,143,111,18,255,0,226,79,254,196,63,253,16,148,44,134,165,151,187,110,251,121,127,193,15,107,230,143,223,207,248,108,235,63,250,10,39,253,245,255,0,220,116,127,195,103,89,255,0,208,81,63,239,175,254,227,175,192,63,248,119,119,252,23,195,254,145,237,226,95,252,73,255,0,216,135,255,0,162,18,143,248,119,119,252,23,195,254,145,237,226,95,252,73,255,0,216,135,255,0,162,18,143,236,26,150,248,117,249,121,127,193,15,107,230,143,223,207,248,108,235,63,250,10,39,253,245,255,0,220,116,127,195,103,89,255,0,208,81,63,239,175,254,227,175,231,19,226,199,236,127,255,0,5,181,248,29,240,179,226,95,198,175,138,95,176,159,137,124,47,240,203,224,255,0,128,60,101,241,71,226,47,137,127,225,163,63,99,173,111,254,17,223,2,124,63,240,230,165,226,207,23,107,191,216,222,29,248,233,119,168,106,255,0,100,240,254,145,168,92,125,150,198,210,234,242,227,236,254,85,173,188,211,186,70,220,47,197,79,129,95,240,87,239,130,154,111,194,125,95,226,103,236,97,226,79,13,105,255,0,28,126,32,252,53,248,87,240,186,113,241,231,246,82,214,79,137,252,123,241,127,197,126,26,240,71,195,189,4,197,160,124,99,186,125,19,251,71,197,30,48,240,229,175,218,181,21,180,178,180,254,210,243,175,174,109,173,225,158,88,179,121,29,88,238,191,45,117,74,223,127,245,176,123,93,181,63,167,95,248,108,235,63,250,10,39,253,245,255,0,220,116,127,195,103,89,255,0,208,81,63,239,175,254,227,175,192,63,248,119,119,252,23,195,254,145,239,226,95,252,73,255,0,216,135,255,0,162,18,143,248,119,119,252,23,195,254,145,237,226,95,252,73,255,0,216,135,255,0,162,18,173,100,21,123,116,93,188,174,30,213,95,73,127,90,31,191,159,240,217,214,127,244,20,79,251,235,255,0,184,232,255,0,134,206,179,255,0,160,162,127,223,95,253,199,95,128,127,240,238,239,248,47,135,253,35,219,196,191,248,147,255,0,177,15,255,0,68,37,31,240,238,239,248,47,135,253,35,219,196,191,248,147,255,0,177,15,255,0,68,37,63,236,26,150,94,239,174,222,65,237,124,209,251,249,255,0,13,157,103,255,0,65,68,255,0,190,191,251,142,143,248,108,235,63,250,10,39,253,245,255,0,220,117,248,7,255,0,14,238,255,0,130,248,127,210,61,188,75,255,0,137,63,251,16,255,0,244,66,81,255,0,14,238,255,0,130,248,127,210,61,188,75,255,0,137,63,251,16,255,0,244,66,81,253,131,82,203,221,215,174,222,65,237,124,209,251,248,63,108,251,63,250,10,39,67,220,31,253,179,226,190,77,253,156,255,0,107,107,77,43,227,15,237,245,124,218,138,32,241,39,237,103,224,237,101,88,183,250,197,135,246,20,253,138,252,62,31,155,78,70,237,5,134,120,229,58,117,39,242,215,254,29,221,255,0,5,240,255,0,164,123,120,151,255,0,18,127,246,33,255,0,232,132,174,39,194,191,240,75,255,0,248,47,111,134,181,223,137,122,210,255,0,193,62,252,66,237,241,7,198,214,30,47,149,35,253,167,255,0,98,80,240,189,151,195,143,135,254,1,17,204,95,246,132,199,154,98,240,60,111,242,130,187,38,64,114,193,171,122,89,52,225,74,181,55,31,226,36,186,116,105,191,200,151,87,88,187,222,223,240,15,178,63,224,185,255,0,180,221,191,196,63,248,37,143,237,69,225,8,175,210,103,213,255,0,225,73,98,48,65,45,246,15,218,43,225,30,168,112,69,178,227,2,200,156,238,29,63,2,87,192,63,180,95,252,18,67,254,11,213,241,231,224,223,140,126,20,92,127,193,63,245,203,72,188,84,60,61,186,230,239,246,157,253,137,205,188,95,216,126,41,209,60,72,60,193,109,251,65,151,59,155,71,85,24,254,38,25,227,52,87,175,151,225,37,133,195,170,77,37,102,223,223,99,41,203,154,77,159,215,255,0,199,79,218,247,225,46,157,255,0,5,30,248,29,227,125,39,195,63,180,199,196,159,0,124,15,253,152,191,111,223,128,127,18,190,36,124,11,253,138,191,108,159,218,23,225,174,137,241,151,199,127,27,191,96,139,205,31,225,238,153,241,23,224,95,192,95,17,104,158,48,241,13,165,223,192,63,140,250,86,191,30,139,127,168,39,133,124,67,240,227,84,240,207,137,228,210,60,67,110,116,198,206,255,0,130,47,254,208,30,9,211,254,11,120,123,246,68,241,119,133,254,62,124,44,253,161,229,241,239,252,20,19,246,135,211,254,30,252,112,253,150,127,105,191,129,7,92,248,57,174,126,223,31,16,252,93,101,226,255,0,12,120,183,227,47,194,45,11,67,241,95,151,225,223,218,55,224,149,197,238,159,166,234,119,90,166,155,255,0,11,2,210,29,70,198,214,120,238,226,183,255,0,45,63,248,42,247,252,165,47,254,10,93,255,0,103,247,251,100,255,0,235,69,252,69,175,223,127,248,50,179,254,82,157,241,243,254,204,7,226,167,254,180,79,236,167,93,113,167,21,136,169,85,124,82,73,63,69,111,243,39,165,143,244,248,162,138,43,112,10,40,162,128,10,40,162,128,56,15,139,31,11,188,9,241,199,225,103,196,191,130,191,20,116,47,248,74,62,25,124,96,248,127,227,47,133,223,17,124,53,253,167,172,104,159,240,145,120,19,226,7,135,117,47,9,248,187,66,254,217,240,238,161,105,168,105,31,107,240,254,173,168,91,253,170,198,238,214,242,223,237,30,109,173,196,51,162,72,188,31,197,63,217,135,224,111,198,189,55,225,62,145,241,55,193,31,240,146,233,255,0,3,190,33,124,53,248,169,240,186,223,254,18,95,23,232,223,240,139,248,247,225,7,138,252,51,227,127,135,122,247,155,225,253,126,209,245,175,236,239,20,120,59,195,151,95,101,212,90,238,202,239,251,59,200,190,183,185,183,154,120,101,247,186,41,52,158,234,246,255,0,135,252,210,0,162,138,41,128,81,69,20,0,81,69,20,0,81,69,20,0,81,69,20,1,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 4780; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

