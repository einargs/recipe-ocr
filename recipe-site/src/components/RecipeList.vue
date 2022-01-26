<template>
  <page-skeleton>
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
    </template>
    <template #header-right>
      <el-button
        @click="goToAdd"
        type="primary">Add Recipe</el-button>
    </template>
    <el-input v-model="query" placeholder="Search..." @change="search" />
      <!-- element-loading-text="Loading..."
      element-loading-background="rgba(0,0,0,0.8)" -->
    <div
      v-loading="loading"
      class="recipe-list">
      <div
        v-for="recipe in recipes"
        :key="recipe.id"
        @click="goToRecipe(recipe)"
        class="recipe-item">
        <span>{{recipe.name}}</span>
        <div class="recipe-item__actions">
          <el-button
            @click.stop="deleteRecipe(recipe)"
            circle>
            <el-icon><i-ep-delete /></el-icon>
          </el-button>
        </div>
      </div>
    </div>
    <el-pagination
      :currentPage="page"
      :page-size="20"
      :total="total"
      @update:current-page="search"
      layout="prev, pager, next" >
    </el-pagination>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import { searchRecipes, deleteRecipe } from "../Api.js"

export default {
  name: 'RecipeList',
  components: {PageSkeleton},
  data() {
    return {
      loading: true,
      query: "",
      page: 1,
      total: 20,
      recipes: [],
    }
  },
  methods: {
    goToRecipe(recipe) {
      this.$router.push({ name: "recipe-view", params: {
        id: recipe.id
      }})
    },

    goToAdd() {
      this.$router.push({ name: "recipe-add" })
    },

    search() {
      searchRecipes(this.$data.query, this.$data.page - 1)
        .then(({total, recipes}) => {
          this.$data.total = total
          this.$data.recipes = recipes
          this.$data.loading = false
        })
    },

    deleteRecipe(recipe) {
      this.$data.loading = true
      deleteRecipe(recipe.id)
        .finally(() => {
          // We search to update the thing-a-ma-bobber
          this.search()
        })
    },
  },
  created() {
    this.search()
  },
}
</script>

<style>
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.recipe-list {
  margin-top: 8px;
}

.recipe-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 8px;
}

.recipe-item:hover {
  background-color: var(--el-border-color-lighter);
}

.item {
  display: flex;
  align-items: center;
  justify-content: flex-start;
  flex-direction: row;
  margin: 10px;
}
</style>
