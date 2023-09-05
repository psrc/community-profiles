library(rsconnect)

# test version ----

deployApp(account = 'psrcwa',
          appName = 'test-community-profiles',
          appTitle = 'TEST Community Profiles')

# official version ----

deployApp(account = 'psrcwa',
          appName = 'community-profiles',
          appTitle = 'Community Profiles')
