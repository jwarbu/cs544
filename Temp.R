ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestBackEnd,group=1)) + 
  geom_line(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Back End roles by gender")

ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestDataEngr,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Data Engineering roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestDevOps,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Dev Ops roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestFrontEnd,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Front End roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestFullStack,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Full Stack roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestGameDev,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Game Dev roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestInfoSec,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Info Sec roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestMobile,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Mobile roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestProjMngr,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in Project Manager roles by gender")


ggplot(s2017.BackendGender,
       aes(x=s2017.BackendGender$Gender,
           y=s2017.BackendGender$JobInterestUX,group=1)) + 
  geom_bar(stat ="identity") + xlab("Gender") + ylab("# interested") +
  ggtitle("Job Interest in UX roles by gender")
