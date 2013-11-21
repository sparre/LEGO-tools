-- OS (spec)
--
-- Root package for operating system interface packages.

-- Written 1996.03.23 by Jacob Sparre Andersen
--   First experiment.
--  1999.09.25 (Jacob Sparre Andersen)
--    Moved exception System_Error from OS.Send_Mail.
--
------------------------------------------------------------------------------

package OS is

   ---------------------------------------------------------------------------
   --  Exceptions:

   System_Error : exception;

   ---------------------------------------------------------------------------

end OS;
