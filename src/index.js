import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

// Firebase App (the core Firebase SDK) is always required and must be listed first
import * as firebase from 'firebase/app'

// If you enabled Analytics in your project, add the Firebase SDK for Analytics
import 'firebase/analytics'

// Add the Firebase products that you want to use
import 'firebase/auth'
import 'firebase/firestore'

const firebaseConfig = {
    apiKey: 'AIzaSyA2YfEsjrWlPrWm4dNP1dD_iy1I4LX_fOk',
    authDomain: 'timetracker-1f2ea.firebaseapp.com',
    databaseURL: 'https://timetracker-1f2ea.firebaseio.com',
    projectId: 'timetracker-1f2ea',
    storageBucket: 'timetracker-1f2ea.appspot.com',
    messagingSenderId: '1011755430384',
    appId: '1:1011755430384:web:3b269dbd40fb0f9aafad35',
    measurementId: 'G-FL3VRT2673',
}

firebase.initializeApp(firebaseConfig)

const db = firebase.firestore()

const app = Elm.Main.init({
    node: document.getElementById('root'),
})

firebase.auth().onAuthStateChanged(function(user) {
    app.ports.fromJS.send({ tag: 'AuthStateChanged', data: user })
})

app.ports.toJS.subscribe(portData => {
    switch (portData.tag) {
        case 'LoginUser':
            firebase
                .auth()
                .signInWithEmailAndPassword(
                    portData.data.email,
                    portData.data.password
                )
                .catch(function(error) {
                    console.log(error)
                    var errorCode = error.code
                    var errorMessage = error.message
                    console.log(errorCode, errorMessage)
                    app.ports.fromJS.send({
                        tag: 'LoginError',
                        data: error,
                    })
                })
            break
        case 'CreateUser':
            firebase
                .auth()
                .createUserWithEmailAndPassword(
                    portData.data.email,
                    portData.data.password
                )
                .catch(function(error) {
                    // TODO: Send info back to Elm
                    var errorCode = error.code
                    var errorMessage = error.message
                    console.log(errorCode, errorMessage)
                    app.ports.fromJS.send({
                        tag: 'SignupError',
                        data: error,
                    })
                })
            break
        case 'SaveData':
            break
        case 'SignOut':
            firebase
                .auth()
                .signOut()
                .then(function() {
                    app.ports.fromJS.send({
                        tag: 'UserSignedOut',
                        data: null,
                    })
                })
                .catch(function(error) {
                    app.ports.fromJS.send({
                        tag: 'SignOutError',
                        data: error,
                    })
                })
            break
        default:
            console.warn('Got an unknown tag')
    }
})

// app.ports.saveData.subscribe((data) => {
//   if (data.id) {
//     db.collection("tracked_days").get(data.id).set(data[0], {merge: true})
//     .then(function(docRef) {
//         console.log("Document written with ID: ", data.id);
//     })
//     .catch(function(error) {
//         console.error("Error adding document: ", error);
//     });
//   }
//   else {
//     db.collection("tracked_days").add(data[0])
//     .then(function(docRef) {
//         console.log("Document written with ID: ", docRef.id);
//         console.log(docRef)
//         app.ports.updateDocument.send(docRef)
//     })
//     .catch(function(error) {
//         console.error("Error adding document: ", error);
//     });
//   }
// });

registerServiceWorker()

function updateDoc(argument) {
    // body...
}
