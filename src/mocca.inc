// the global defs for mocca

{$H+}

{$ifdef DEBUG_ALL}
  {$define DEBUG_EMC} 
  {$define DEBUG_CONFIG}
  {$define DEBUG_INI}
  {$define PRINT_CANON}
  {$define DEBUG_GL}
{$endif}

{$ifdef LCLGTK2}
  {$define OWNGL}
{$endif}

{$define HAS_SLEEP}

// {$define LINESMOOTH}




