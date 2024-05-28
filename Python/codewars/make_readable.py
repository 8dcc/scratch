# ---------------------------------------------------------------------------------
# The one I made

def make_readable(seconds):
    hours = str(seconds//3600) if len(str(seconds//3600)) != 1 else "0" + str(seconds//3600)
    min = str((seconds%3600)//60) if len(str((seconds%3600)//60)) != 1 else "0" + str((seconds%3600)//60)
    rem = str((seconds%3600)%60) if len(str((seconds%3600)%60)) != 1 else "0" + str((seconds%3600)%60)
    return f"{hours}:{min}:{rem}"
      
# ---------------------------------------------------------------------------------
# The one I found (you need to convert days to hours and add a 0 to the hours):

import datetime
def make_readable(seconds):
    return str(datetime.timedelta(seconds=seconds))
  
# ---------------------------------------------------------------------------------
