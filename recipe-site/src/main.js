import { createApp } from 'vue'
import { createRouter, createWebHashHistory } from 'vue-router'
import { ElLoading } from "element-plus"
import App from './App.vue'
import RecipeList from './components/RecipeList.vue'
import RecipeView from './components/RecipeView.vue'
import RecipeAdd from './components/RecipeAdd.vue'
import RecipeEdit from './components/RecipeEdit.vue'

const app = createApp(App)

app.use(ElLoading)

const routes = [
  { path: '/', redirect: '/recipes/' },
  { path: '/recipes/', name: "recipe-list", component: RecipeList },
  { path: '/recipes/new/', name: "recipe-add", component: RecipeAdd },
  { path: '/recipes/:id/', name: "recipe-view", component: RecipeView },
  { path: '/recipes/:id/edit/', name: "recipe-edit", component: RecipeEdit },
]

const router = createRouter({
  history: createWebHashHistory(),
  routes,
})

app.use(router)

app.mount('#app')
