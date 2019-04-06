module Hp.GitHub.User where

import Data.Aeson (FromJSON)


-- {
--     "avatar_url": "https://avatars2.githubusercontent.com/u/1074598?v=4",
--     "bio": null,
--     "blog": "https://twitter.com/mitchellsalad",
--     "company": "@Sentenai",
--     "created_at": "2011-09-23T17:22:17Z",
--     "email": "mitchellwrosen@gmail.com",
--     "events_url": "https://api.github.com/users/mitchellwrosen/events{/privacy}",
--     "followers": 62,
--     "followers_url": "https://api.github.com/users/mitchellwrosen/followers",
--     "following": 189,
--     "following_url": "https://api.github.com/users/mitchellwrosen/following{/other_user}",
--     "gists_url": "https://api.github.com/users/mitchellwrosen/gists{/gist_id}",
--     "gravatar_id": "",
--     "hireable": null,
--     "html_url": "https://github.com/mitchellwrosen",
--     "id": 1074598,
--     "location": "Cambridge, MA",
--     "login": "mitchellwrosen",
--     "name": "Mitchell Rosen",
--     "node_id": "MDQ6VXNlcjEwNzQ1OTg=",
--     "organizations_url": "https://api.github.com/users/mitchellwrosen/orgs",
--     "public_gists": 13,
--     "public_repos": 322,
--     "received_events_url": "https://api.github.com/users/mitchellwrosen/received_events",
--     "repos_url": "https://api.github.com/users/mitchellwrosen/repos",
--     "site_admin": false,
--     "starred_url": "https://api.github.com/users/mitchellwrosen/starred{/owner}{/repo}",
--     "subscriptions_url": "https://api.github.com/users/mitchellwrosen/subscriptions",
--     "type": "User",
--     "updated_at": "2019-04-02T15:50:42Z",
--     "url": "https://api.github.com/users/mitchellwrosen"
-- }
data GitHubUser
  = GitHubUser
  { login :: Text
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
