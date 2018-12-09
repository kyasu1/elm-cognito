import '../css/style.scss';
import { Elm } from '../src/Main.elm';
import { CognitoUserPool, CognitoUserAttribute, CognitoUser, AuthenticationDetails } from 'amazon-cognito-identity-js';

// const UserPoolId = 'ap-northeast-1:3d7e92f1-1123-4300-8673-13a7c7141119';
const UserPoolId = 'ap-northeast-1_5lHoc28av';
const ClientId = '4aajp1e5cuqj9hooej2689q0ju';

const poolData = {
  UserPoolId,
  ClientId
}
const userPool = new CognitoUserPool(poolData);
const currentUser = userPool.getCurrentUser();

if (currentUser) {
  currentUser.getSession(function(err, session) {
    if (err) {
      console.log('Failed to get currentSession')
      start(null);
    } else {
      console.log('Retrieved Session : ', session)
      start(session);
    }
  })
} else {
  console.log('Failed to get CurrentUser')
  start(null);
}


function start (session) {
  const app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
      session
    },
  });

  app.ports.elmToJs.subscribe(function( {tag, value} ) {
    console.log(tag)
    switch (tag) {
      case 'SignIn':
        const userData = {
          Username: value.username,
          Pool: userPool
        };
        const cognitoUser = new CognitoUser(userData);
        const authenticationDetails = new AuthenticationDetails({
          Username: value.username,
          Password: value.password
        });
        cognitoUser.authenticateUser(authenticationDetails, {
          onSuccess: function(result) {
            app.ports.signInResponse.send({
              accessToken: result.getAccessToken().getJwtToken(),
              refreshToken: result.getRefreshToken().getToken(),
              idToken: result.getIdToken().getJwtToken(),
              clockDrift: result.getClockDrift()
            });
          },
          onFailure: function(error) {
            app.ports.signInResponse.send(error);
          }
        });
        break;
    }
  });
}

function toElm(app, tag, value) {
  console.log("In toElm : ", tag, " : ", value);
  app.ports.jsToElm.send( { tag, value })
}
